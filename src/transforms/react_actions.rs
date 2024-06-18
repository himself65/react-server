use std::{
	collections::BTreeMap,
	convert::{TryFrom, TryInto},
	mem::take,
};

use hex::encode as hex_encode;
use serde::Deserialize;
use sha1::{Digest, Sha1};
use swc_core::{
	common::{
		comments::{Comment, CommentKind, Comments},
		errors::HANDLER,
		util::take::Take,
		BytePos, FileName, DUMMY_SP,
	},
	ecma::{
		ast::*,
		atoms::JsWord,
		utils::{private_ident, quote_ident, ExprFactory},
		visit::{as_folder, noop_visit_mut_type, Fold, VisitMut, VisitMutWith},
	},
};
use swc_core::common::Span;


#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct Config {
	pub is_react_server_layer: bool,
	pub enabled: bool,
}

/// A mapping of hashed action id to the action's exported function name.
// Using BTreeMap to ensure the order of the actions is deterministic.
pub type ActionsMap = BTreeMap<String, String>;

#[tracing::instrument(level = tracing::Level::TRACE, skip_all)]
pub fn server_actions<C: Comments>(
	file_name: &FileName,
	config: Config,
	comments: C,
) -> impl VisitMut + Fold {
	as_folder(ServerActions {
		config,
		comments,
		file_name: file_name.to_string(),
		start_pos: BytePos(0),
		in_action_file: false,
		in_export_decl: false,
		in_default_export_decl: false,
		in_callee: false,
		has_action: false,

		reference_index: 0,
		in_module_level: true,
		should_track_names: false,

		names: Default::default(),
		declared_idents: Default::default(),

		exported_idents: Default::default(),

		// This flag allows us to rewrite `function foo() {}` to `const foo = createProxy(...)`.
		rewrite_fn_decl_to_proxy_decl: None,
		rewrite_default_fn_expr_to_proxy_expr: None,
		rewrite_expr_to_proxy_expr: None,

		annotations: Default::default(),
		extra_items: Default::default(),
		export_actions: Default::default(),
	})
}

/// Serializes the Server Actions into a magic comment prefixed by
/// `__next_internal_action_entry_do_not_use__`.
fn generate_server_actions_comment(actions: ActionsMap) -> String {
	format!(
		" __next_internal_action_entry_do_not_use__ {} ",
		serde_json::to_string(&actions).unwrap()
	)
}

struct ServerActions<C: Comments> {
	#[allow(unused)]
	config: Config,
	file_name: String,
	comments: C,

	start_pos: BytePos,
	in_action_file: bool,
	in_export_decl: bool,
	in_default_export_decl: bool,
	in_callee: bool,
	has_action: bool,

	reference_index: u32,
	in_module_level: bool,
	should_track_names: bool,

	names: Vec<Name>,
	declared_idents: Vec<Id>,

	// This flag allows us to rewrite `function foo() {}` to `const foo = createProxy(...)`.
	rewrite_fn_decl_to_proxy_decl: Option<VarDecl>,
	rewrite_default_fn_expr_to_proxy_expr: Option<Box<Expr>>,
	rewrite_expr_to_proxy_expr: Option<Box<Expr>>,

	// (ident, export name)
	exported_idents: Vec<(Id, String)>,

	annotations: Vec<Stmt>,
	extra_items: Vec<ModuleItem>,
	export_actions: Vec<String>,
}

impl<C: Comments> ServerActions<C> {
	// Check if the function or arrow function is an action function
	fn get_action_info(
		&mut self,
		maybe_body: Option<&mut BlockStmt>,
		remove_directive: bool,
	) -> bool {
		let mut is_action_fn = false;

		if self.in_action_file && self.in_export_decl {
			// All export functions in a server file are actions
			is_action_fn = true;
		} else {
			// Check if the function has `"use server"`
			if let Some(body) = maybe_body {
				let mut action_span = None;
				remove_server_directive_index_in_fn(
					&mut body.stmts,
					remove_directive,
					&mut is_action_fn,
					&mut action_span,
					self.config.enabled,
				);

				if is_action_fn && !self.config.is_react_server_layer && !self.in_action_file {
					HANDLER.with(|handler| {
						handler
							.struct_span_err(
								action_span.unwrap_or(body.span),
								"It is not allowed to define inline \"use server\" annotated Server Actions in Client Components.\nTo use Server Actions in a Client Component, you can either export them from a separate file with \"use server\" at the top, or pass them down through props from a Server Component.\n\nRead more: https://nextjs.org/docs/app/api-reference/functions/server-actions#with-client-components\n",
							)
							.emit()
					});
				}
			}
		}

		is_action_fn
	}

	fn maybe_hoist_and_create_proxy(
		&mut self,
		ids_from_closure: Vec<Name>,
		function: Option<&mut Box<Function>>,
		arrow: Option<&mut ArrowExpr>,
	) -> Option<Box<Expr>> {
		let action_name: JsWord = gen_ident(&mut self.reference_index);
		let action_ident = private_ident!(action_name.clone());
		let export_name: JsWord = action_name;

		self.has_action = true;
		self.export_actions.push(export_name.to_string());

		if let Some(a) = arrow {
			let register_action_expr = annotate_ident_as_action(
				action_ident.clone(),
				ids_from_closure
					.iter()
					.cloned()
					.map(|id| Some(id.as_arg()))
					.collect(),
				&self.file_name,
				export_name.to_string(),
			);

			if let BlockStmtOrExpr::BlockStmt(block) = &mut *a.body {
				block.visit_mut_with(&mut ClosureReplacer {
					used_ids: &ids_from_closure,
				});
			}

			// export const $ACTION_myAction = async () => {}
			let mut new_params: Vec<Param> = vec![];
			let mut new_body: BlockStmtOrExpr = *a.body.clone();

			if !ids_from_closure.is_empty() {
				// First argument is the encrypted closure variables
				new_params.push(Param {
					span: DUMMY_SP,
					decorators: vec![],
					pat: Pat::Ident(Ident::new("$$ACTION_CLOSURE_BOUND".into(), DUMMY_SP).into()),
				});

				// Also prepend the decryption decl into the body.
				// var [arg1, arg2, arg3] = await decryptActionBoundArgs(actionId,
				// $$ACTION_CLOSURE_BOUND)
				let mut pats = vec![];
				for i in 0..ids_from_closure.len() {
					pats.push(Some(Pat::Ident(
						Ident::new(format!("$$ACTION_ARG_{}", i).into(), DUMMY_SP).into(),
					)));
				}
				let decryption_decl = VarDecl {
					span: DUMMY_SP,
					kind: VarDeclKind::Var,
					declare: false,
					decls: vec![VarDeclarator {
						span: DUMMY_SP,
						name: Pat::Array(ArrayPat {
							span: DUMMY_SP,
							elems: pats,
							optional: false,
							type_ann: None,
						}),
						init: Some(Box::new(Expr::Await(AwaitExpr {
							span: DUMMY_SP,
							arg: Box::new(Expr::Call(CallExpr {
								span: DUMMY_SP,
								callee: quote_ident!("decryptActionBoundArgs").as_callee(),
								args: vec![
									generate_action_id(&self.file_name, &export_name).as_arg(),
									quote_ident!("$$ACTION_CLOSURE_BOUND").as_arg(),
								],
								type_args: None,
							})),
						}))),
						definite: Default::default(),
					}],
				};

				match &mut new_body {
					BlockStmtOrExpr::BlockStmt(body) => {
						body.stmts.insert(0, decryption_decl.into());
					}
					BlockStmtOrExpr::Expr(body_expr) => {
						new_body = BlockStmtOrExpr::BlockStmt(BlockStmt {
							span: DUMMY_SP,
							stmts: vec![
								decryption_decl.into(),
								Stmt::Return(ReturnStmt {
									span: DUMMY_SP,
									arg: Some(body_expr.take()),
								}),
							],
						});
					}
				}
			}

			for p in a.params.iter() {
				new_params.push(Param {
					span: DUMMY_SP,
					decorators: vec![],
					pat: p.clone(),
				});
			}

			// Create the action export decl from the arrow function
			self.extra_items
				.push(ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
					span: DUMMY_SP,
					decl: FnDecl {
						ident: action_ident.clone(),
						function: Box::new(Function {
							params: new_params,
							body: match new_body {
								BlockStmtOrExpr::BlockStmt(body) => Some(body),
								BlockStmtOrExpr::Expr(expr) => Some(BlockStmt {
									span: DUMMY_SP,
									stmts: vec![Stmt::Return(ReturnStmt {
										span: DUMMY_SP,
										arg: Some(expr),
									})],
								}),
							},
							decorators: vec![],
							span: DUMMY_SP,
							is_generator: false,
							is_async: true,
							type_params: None,
							return_type: None,
						}),
						declare: Default::default(),
					}
						.into(),
				})));

			return Some(Box::new(register_action_expr.clone()));
		} else if let Some(f) = function {
			let register_action_expr = annotate_ident_as_action(
				action_ident.clone(),
				ids_from_closure
					.iter()
					.cloned()
					.map(|id| Some(id.as_arg()))
					.collect(),
				&self.file_name,
				export_name.to_string(),
			);

			f.body.visit_mut_with(&mut ClosureReplacer {
				used_ids: &ids_from_closure,
			});

			// export async function $ACTION_myAction () {}
			let mut new_params: Vec<Param> = vec![];
			let mut new_body: Option<BlockStmt> = f.body.clone();

			// add params from closure collected ids
			if !ids_from_closure.is_empty() {
				// First argument is the encrypted closure variables
				new_params.push(Param {
					span: DUMMY_SP,
					decorators: vec![],
					pat: Pat::Ident(Ident::new("$$ACTION_CLOSURE_BOUND".into(), DUMMY_SP).into()),
				});

				// Also prepend the decryption decl into the body.
				// var [arg1, arg2, arg3] = await decryptActionBoundArgs(actionId,
				// $$ACTION_CLOSURE_BOUND)
				let mut pats = vec![];
				for i in 0..ids_from_closure.len() {
					pats.push(Some(Pat::Ident(
						Ident::new(format!("$$ACTION_ARG_{}", i).into(), DUMMY_SP).into(),
					)));
				}
				let decryption_decl = VarDecl {
					span: DUMMY_SP,
					kind: VarDeclKind::Var,
					declare: false,
					decls: vec![VarDeclarator {
						span: DUMMY_SP,
						name: Pat::Array(ArrayPat {
							span: DUMMY_SP,
							elems: pats,
							optional: false,
							type_ann: None,
						}),
						init: Some(Box::new(Expr::Await(AwaitExpr {
							span: DUMMY_SP,
							arg: Box::new(Expr::Call(CallExpr {
								span: DUMMY_SP,
								callee: quote_ident!("decryptActionBoundArgs").as_callee(),
								args: vec![
									generate_action_id(&self.file_name, &export_name).as_arg(),
									quote_ident!("$$ACTION_CLOSURE_BOUND").as_arg(),
								],
								type_args: None,
							})),
						}))),
						definite: Default::default(),
					}],
				};

				if let Some(body) = &mut new_body {
					body.stmts.insert(0, decryption_decl.into());
				} else {
					new_body = Some(BlockStmt {
						span: DUMMY_SP,
						stmts: vec![decryption_decl.into()],
					});
				}
			}

			for p in f.params.iter() {
				new_params.push(p.clone());
			}

			self.extra_items
				.push(ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
					span: DUMMY_SP,
					decl: FnDecl {
						ident: action_ident.clone(),
						function: Box::new(Function {
							params: new_params,
							body: new_body,
							..*f.take()
						}),
						declare: Default::default(),
					}
						.into(),
				})));

			return Some(Box::new(register_action_expr));
		}

		None
	}
}

impl<C: Comments> VisitMut for ServerActions<C> {
	fn visit_mut_export_decl(&mut self, decl: &mut ExportDecl) {
		let old = self.in_export_decl;
		self.in_export_decl = true;
		decl.decl.visit_mut_with(self);
		self.in_export_decl = old;
	}

	fn visit_mut_export_default_decl(&mut self, decl: &mut ExportDefaultDecl) {
		let old = self.in_export_decl;
		let old_default = self.in_default_export_decl;
		self.in_export_decl = true;
		self.in_default_export_decl = true;
		self.rewrite_default_fn_expr_to_proxy_expr = None;
		decl.decl.visit_mut_with(self);
		self.in_export_decl = old;
		self.in_default_export_decl = old_default;
	}

	fn visit_mut_export_default_expr(&mut self, expr: &mut ExportDefaultExpr) {
		let old = self.in_export_decl;
		let old_default = self.in_default_export_decl;
		self.in_export_decl = true;
		self.in_default_export_decl = true;
		expr.expr.visit_mut_with(self);
		self.in_export_decl = old;
		self.in_default_export_decl = old_default;
	}

	fn visit_mut_fn_expr(&mut self, f: &mut FnExpr) {
		let is_action_fn = self.get_action_info(f.function.body.as_mut(), true);

		let declared_idents_until = self.declared_idents.len();
		let current_names = take(&mut self.names);

		// Visit children
		{
			let old_in_module = self.in_module_level;
			let old_should_track_names = self.should_track_names;
			let old_in_export_decl = self.in_export_decl;
			let old_in_default_export_decl = self.in_default_export_decl;
			self.in_module_level = false;
			self.should_track_names = is_action_fn || self.should_track_names;
			self.in_export_decl = false;
			self.in_default_export_decl = false;
			f.visit_mut_children_with(self);
			self.in_module_level = old_in_module;
			self.should_track_names = old_should_track_names;
			self.in_export_decl = old_in_export_decl;
			self.in_default_export_decl = old_in_default_export_decl;
		}

		let mut child_names = if self.should_track_names {
			let names = take(&mut self.names);
			self.names = current_names;
			self.names.extend(names.iter().cloned());
			names
		} else {
			take(&mut self.names)
		};

		if !is_action_fn {
			return;
		}

		if !f.function.is_async {
			HANDLER.with(|handler| {
				handler
					.struct_span_err(f.function.span, "Server actions must be async functions")
					.emit();
			});
		}

		if !(self.in_action_file && self.in_export_decl) {
			// It's an action function. If it doesn't have a name, give it one.
			match f.ident.as_mut() {
				None => {
					let action_name = gen_ident(&mut self.reference_index);
					let ident = Ident::new(action_name, DUMMY_SP);
					f.ident.insert(ident)
				}
				Some(i) => i,
			};

			// Collect all the identifiers defined inside the closure and used
			// in the action function. With deduplication.
			retain_names_from_declared_idents(
				&mut child_names,
				&self.declared_idents[..declared_idents_until],
			);

			let maybe_new_expr =
				self.maybe_hoist_and_create_proxy(child_names, Some(&mut f.function), None);

			if self.in_default_export_decl {
				// This function expression is also the default export:
				// `export default async function() {}`
				// This specific case (default export) isn't handled by `visit_mut_expr`.
				// Replace the original function expr with a action proxy expr.
				self.rewrite_default_fn_expr_to_proxy_expr = maybe_new_expr;
			} else {
				self.rewrite_expr_to_proxy_expr = maybe_new_expr;
			}
		}
	}

	fn visit_mut_decl(&mut self, d: &mut Decl) {
		self.rewrite_fn_decl_to_proxy_decl = None;
		d.visit_mut_children_with(self);

		if let Some(decl) = &self.rewrite_fn_decl_to_proxy_decl {
			*d = (*decl).clone().into();
		}

		self.rewrite_fn_decl_to_proxy_decl = None;
	}

	fn visit_mut_fn_decl(&mut self, f: &mut FnDecl) {
		let is_action_fn = self.get_action_info(f.function.body.as_mut(), true);

		let declared_idents_until = self.declared_idents.len();
		let current_names = take(&mut self.names);

		{
			// Visit children
			let old_in_module = self.in_module_level;
			let old_should_track_names = self.should_track_names;
			let old_in_export_decl = self.in_export_decl;
			let old_in_default_export_decl = self.in_default_export_decl;
			self.in_module_level = false;
			self.should_track_names = is_action_fn || self.should_track_names;
			self.in_export_decl = false;
			self.in_default_export_decl = false;
			f.visit_mut_children_with(self);
			self.in_module_level = old_in_module;
			self.should_track_names = old_should_track_names;
			self.in_export_decl = old_in_export_decl;
			self.in_default_export_decl = old_in_default_export_decl;
		}

		let mut child_names = if self.should_track_names {
			let names = take(&mut self.names);
			self.names = current_names;
			self.names.extend(names.iter().cloned());
			names
		} else {
			take(&mut self.names)
		};

		if !is_action_fn {
			return;
		}

		if !f.function.is_async {
			HANDLER.with(|handler| {
				handler
					.struct_span_err(f.ident.span, "Server actions must be async functions")
					.emit();
			});
		}

		if !(self.in_action_file && self.in_export_decl) {
			// Collect all the identifiers defined inside the closure and used
			// in the action function. With deduplication.
			retain_names_from_declared_idents(
				&mut child_names,
				&self.declared_idents[..declared_idents_until],
			);

			let maybe_new_expr =
				self.maybe_hoist_and_create_proxy(child_names, Some(&mut f.function), None);

			// Replace the original function declaration with a action proxy declaration
			// expr.
			self.rewrite_fn_decl_to_proxy_decl = Some(VarDecl {
				span: DUMMY_SP,
				kind: VarDeclKind::Var,
				declare: false,
				decls: vec![VarDeclarator {
					span: DUMMY_SP,
					name: Pat::Ident(f.ident.clone().into()),
					init: maybe_new_expr,
					definite: false,
				}],
			});
		}
	}

	fn visit_mut_method_prop(&mut self, m: &mut MethodProp) {
		let old_in_export_decl = self.in_export_decl;
		let old_in_default_export_decl = self.in_default_export_decl;
		self.in_export_decl = false;
		self.in_default_export_decl = false;
		m.visit_mut_children_with(self);
		self.in_export_decl = old_in_export_decl;
		self.in_default_export_decl = old_in_default_export_decl;
	}

	fn visit_mut_class_method(&mut self, m: &mut ClassMethod) {
		let old_in_export_decl = self.in_export_decl;
		let old_in_default_export_decl = self.in_default_export_decl;
		self.in_export_decl = false;
		self.in_default_export_decl = false;
		m.visit_mut_children_with(self);
		self.in_export_decl = old_in_export_decl;
		self.in_default_export_decl = old_in_default_export_decl;
	}

	fn visit_mut_arrow_expr(&mut self, a: &mut ArrowExpr) {
		// Arrow expressions need to be visited in prepass to determine if it's
		// an action function or not.
		let is_action_fn = self.get_action_info(
			if let BlockStmtOrExpr::BlockStmt(block) = &mut *a.body {
				Some(block)
			} else {
				None
			},
			true,
		);

		let declared_idents_until = self.declared_idents.len();
		let current_names = take(&mut self.names);

		{
			// Visit children
			let old_in_module = self.in_module_level;
			let old_should_track_names = self.should_track_names;
			let old_in_export_decl = self.in_export_decl;
			let old_in_default_export_decl = self.in_default_export_decl;
			self.in_module_level = false;
			self.should_track_names = is_action_fn || self.should_track_names;
			self.in_export_decl = false;
			self.in_default_export_decl = false;
			{
				for n in &mut a.params {
					collect_idents_in_pat(n, &mut self.declared_idents);
				}
			}
			a.visit_mut_children_with(self);
			self.in_module_level = old_in_module;
			self.should_track_names = old_should_track_names;
			self.in_export_decl = old_in_export_decl;
			self.in_default_export_decl = old_in_default_export_decl;
		}

		let mut child_names = if self.should_track_names {
			let names = take(&mut self.names);
			self.names = current_names;
			self.names.extend(names.iter().cloned());
			names
		} else {
			take(&mut self.names)
		};

		if !is_action_fn {
			return;
		}

		if !a.is_async && !self.in_action_file {
			HANDLER.with(|handler| {
				handler
					.struct_span_err(a.span, "Server actions must be async functions")
					.emit();
			});
		}

		// Collect all the identifiers defined inside the closure and used
		// in the action function. With deduplication.
		retain_names_from_declared_idents(
			&mut child_names,
			&self.declared_idents[..declared_idents_until],
		);

		let maybe_new_expr = self.maybe_hoist_and_create_proxy(child_names, None, Some(a));
		self.rewrite_expr_to_proxy_expr = maybe_new_expr;
	}

	fn visit_mut_module(&mut self, m: &mut Module) {
		self.start_pos = m.span.lo;
		m.visit_mut_children_with(self);
	}

	fn visit_mut_stmt(&mut self, n: &mut Stmt) {
		n.visit_mut_children_with(self);

		if self.in_module_level {
			return;
		}

		// If it's a closure (not in the module level), we need to collect
		// identifiers defined in the closure.
		collect_decl_idents_in_stmt(n, &mut self.declared_idents);
	}

	fn visit_mut_param(&mut self, n: &mut Param) {
		n.visit_mut_children_with(self);

		if self.in_module_level {
			return;
		}

		collect_idents_in_pat(&n.pat, &mut self.declared_idents);
	}

	fn visit_mut_prop_or_spread(&mut self, n: &mut PropOrSpread) {
		if !self.in_module_level && self.should_track_names {
			if let PropOrSpread::Prop(box Prop::Shorthand(i)) = n {
				self.names.push(Name::from(&*i));
				self.should_track_names = false;
				n.visit_mut_children_with(self);
				self.should_track_names = true;
				return;
			}
		}

		n.visit_mut_children_with(self);
	}

	fn visit_mut_callee(&mut self, n: &mut Callee) {
		let old_in_callee = self.in_callee;
		self.in_callee = true;
		n.visit_mut_children_with(self);
		self.in_callee = old_in_callee;
	}

	fn visit_mut_expr(&mut self, n: &mut Expr) {
		if !self.in_module_level && self.should_track_names {
			if let Ok(mut name) = Name::try_from(&*n) {
				if self.in_callee {
					// This is a callee i.e. `foo.bar()`,
					// we need to track the actual value instead of the method name.
					if !name.1.is_empty() {
						name.1.pop();
					}
				}

				self.names.push(name);
				self.should_track_names = false;
				n.visit_mut_children_with(self);
				self.should_track_names = true;
				return;
			}
		}

		self.rewrite_expr_to_proxy_expr = None;
		n.visit_mut_children_with(self);
		if let Some(expr) = &self.rewrite_expr_to_proxy_expr {
			*n = (**expr).clone();
			self.rewrite_expr_to_proxy_expr = None;
		}
	}

	fn visit_mut_module_items(&mut self, stmts: &mut Vec<ModuleItem>) {
		remove_server_directive_index_in_module(
			stmts,
			&mut self.in_action_file,
			&mut self.has_action,
			self.config.enabled,
		);

		let old_annotations = self.annotations.take();
		let mut new = Vec::with_capacity(stmts.len());

		for mut stmt in stmts.take() {
			// For action file, it's not allowed to export things other than async
			// functions.
			if self.in_action_file {
				let mut disallowed_export_span = DUMMY_SP;

				// Currrently only function exports are allowed.
				match &mut stmt {
					ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl { decl, span })) => {
						match decl {
							Decl::Fn(f) => {
								// export function foo() {}
								self.exported_idents
									.push((f.ident.to_id(), f.ident.sym.to_string()));
							}
							Decl::Var(var) => {
								// export const foo = 1
								let mut ids: Vec<Id> = Vec::new();
								collect_idents_in_var_decls(&var.decls, &mut ids);
								self.exported_idents.extend(
									ids.into_iter().map(|id| (id.clone(), id.0.to_string())),
								);

								for decl in &mut var.decls {
									if let Some(init) = &decl.init {
										if let Expr::Lit(_) = &**init {
											// It's not allowed to export any literal.
											disallowed_export_span = *span;
										}
									}
								}
							}
							_ => {
								disallowed_export_span = *span;
							}
						}
					}
					ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(named)) => {
						if named.src.is_some() {
							disallowed_export_span = named.span;
						} else {
							for spec in &mut named.specifiers {
								if let ExportSpecifier::Named(ExportNamedSpecifier {
									                              orig: ModuleExportName::Ident(ident),
									                              exported,
									                              ..
								                              }) = spec
								{
									if let Some(export_name) = exported {
										if let ModuleExportName::Ident(Ident { sym, .. }) =
											export_name
										{
											// export { foo as bar }
											self.exported_idents
												.push((ident.to_id(), sym.to_string()));
										} else if let ModuleExportName::Str(str) = export_name {
											// export { foo as "bar" }
											self.exported_idents
												.push((ident.to_id(), str.value.to_string()));
										}
									} else {
										// export { foo }
										self.exported_idents
											.push((ident.to_id(), ident.sym.to_string()));
									}
								} else {
									disallowed_export_span = named.span;
								}
							}
						}
					}
					ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultDecl(ExportDefaultDecl {
						                                                     decl,
						                                                     span,
						                                                     ..
					                                                     })) => match decl {
						DefaultDecl::Fn(f) => {
							if let Some(ident) = &f.ident {
								// export default function foo() {}
								self.exported_idents.push((ident.to_id(), "default".into()));
							} else {
								// export default function() {}
								let new_ident =
									Ident::new(gen_ident(&mut self.reference_index), DUMMY_SP);
								f.ident = Some(new_ident.clone());
								self.exported_idents
									.push((new_ident.to_id(), "default".into()));
							}
						}
						_ => {
							disallowed_export_span = *span;
						}
					},
					ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultExpr(default_expr)) => {
						match &mut *default_expr.expr {
							Expr::Fn(_f) => {}
							Expr::Arrow(arrow) => {
								if !arrow.is_async {
									disallowed_export_span = default_expr.span;
								} else {
									// export default async () => {}
									let new_ident =
										Ident::new(gen_ident(&mut self.reference_index), DUMMY_SP);

									self.exported_idents
										.push((new_ident.to_id(), "default".into()));

									*default_expr.expr = attach_name_to_expr(
										new_ident,
										Expr::Arrow(arrow.clone()),
										&mut self.extra_items,
									);
								}
							}
							Expr::Ident(ident) => {
								// export default foo
								self.exported_idents.push((ident.to_id(), "default".into()));
							}
							Expr::Call(call) => {
								// export default fn()
								let new_ident =
									Ident::new(gen_ident(&mut self.reference_index), DUMMY_SP);

								self.exported_idents
									.push((new_ident.to_id(), "default".into()));

								*default_expr.expr = attach_name_to_expr(
									new_ident,
									Expr::Call(call.clone()),
									&mut self.extra_items,
								);
							}
							_ => {
								disallowed_export_span = default_expr.span;
							}
						}
					}
					ModuleItem::ModuleDecl(ModuleDecl::ExportAll(ExportAll { span, .. })) => {
						disallowed_export_span = *span;
					}
					_ => {}
				}

				if disallowed_export_span != DUMMY_SP {
					HANDLER.with(|handler| {
						handler
							.struct_span_err(
								disallowed_export_span,
								"Only async functions are allowed to be exported in a \"use \
                                 server\" file.",
							)
							.emit();
					});
				}
			}

			stmt.visit_mut_with(self);

			let mut new_stmt = stmt;

			if let Some(expr) = &self.rewrite_default_fn_expr_to_proxy_expr {
				// If this happens, we need to replace the statement with a default export expr.
				new_stmt =
					ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultExpr(ExportDefaultExpr {
						span: DUMMY_SP,
						expr: expr.clone(),
					}));
				self.rewrite_default_fn_expr_to_proxy_expr = None;
			}

			if self.config.is_react_server_layer || !self.in_action_file {
				new.push(new_stmt);
				new.extend(self.annotations.drain(..).map(ModuleItem::Stmt));
				new.append(&mut self.extra_items);
			}
		}

		// If it's a "use server" file, all exports need to be annotated as actions.
		if self.in_action_file {
			// If it's compiled in the client layer, each export field needs to be
			// wrapped by a reference creation call.
			let create_ref_ident = private_ident!("createServerReference");
			if !self.config.is_react_server_layer {
				// import { createServerReference } from
				// 'private-next-rsc-action-client-wrapper'
				// createServerReference("action_id")
				new.push(ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
					span: DUMMY_SP,
					specifiers: vec![ImportSpecifier::Named(ImportNamedSpecifier {
						span: DUMMY_SP,
						local: create_ref_ident.clone(),
						imported: None,
						is_type_only: false,
					})],
					src: Box::new(Str {
						span: DUMMY_SP,
						value: "private-next-rsc-action-client-wrapper".into(),
						raw: None,
					}),
					type_only: false,
					with: None,
					phase: Default::default(),
				})));
			}

			for (id, export_name) in self.exported_idents.iter() {
				let ident = Ident::new(id.0.clone(), DUMMY_SP.with_ctxt(id.1));

				if !self.config.is_react_server_layer {
					let action_id = generate_action_id(&self.file_name, export_name);

					if export_name == "default" {
						let export_expr = ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultExpr(
							ExportDefaultExpr {
								span: DUMMY_SP,
								expr: Box::new(Expr::Call(CallExpr {
									span: DUMMY_SP,
									callee: Callee::Expr(Box::new(Expr::Ident(
										create_ref_ident.clone(),
									))),
									args: vec![action_id.as_arg()],
									type_args: None,
								})),
							},
						));
						new.push(export_expr);
					} else {
						let export_expr =
							ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
								span: DUMMY_SP,
								decl: Decl::Var(Box::new(VarDecl {
									span: DUMMY_SP,
									kind: VarDeclKind::Var,
									declare: false,
									decls: vec![VarDeclarator {
										span: DUMMY_SP,
										name: Pat::Ident(
											Ident::new(export_name.clone().into(), DUMMY_SP).into(),
										),
										init: Some(Box::new(Expr::Call(CallExpr {
											span: DUMMY_SP,
											callee: Callee::Expr(Box::new(Expr::Ident(
												create_ref_ident.clone(),
											))),
											args: vec![action_id.as_arg()],
											type_args: None,
										}))),
										definite: false,
									}],
								})),
							}));
						new.push(export_expr);
					}
				} else {
					self.annotations.push(Stmt::Expr(ExprStmt {
						span: DUMMY_SP,
						expr: Box::new(annotate_ident_as_action(
							ident.clone(),
							Vec::new(),
							&self.file_name,
							export_name.to_string(),
						)),
					}));
				}
			}

			if self.config.is_react_server_layer {
				new.append(&mut self.extra_items);

				// Ensure that the exports are valid by appending a check
				// import { ensureServerEntryExports } from 'private-next-rsc-action-validate'
				// ensureServerEntryExports([action1, action2, ...])
				let ensure_ident = private_ident!("ensureServerEntryExports");
				new.push(ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
					span: DUMMY_SP,
					specifiers: vec![ImportSpecifier::Named(ImportNamedSpecifier {
						span: DUMMY_SP,
						local: ensure_ident.clone(),
						imported: None,
						is_type_only: false,
					})],
					src: Box::new(Str {
						span: DUMMY_SP,
						value: "private-next-rsc-action-validate".into(),
						raw: None,
					}),
					type_only: false,
					with: None,
					phase: Default::default(),
				})));
				new.push(ModuleItem::Stmt(Stmt::Expr(ExprStmt {
					span: DUMMY_SP,
					expr: Box::new(Expr::Call(CallExpr {
						span: DUMMY_SP,
						callee: Callee::Expr(Box::new(Expr::Ident(ensure_ident))),
						args: vec![ExprOrSpread {
							spread: None,
							expr: Box::new(Expr::Array(ArrayLit {
								span: DUMMY_SP,
								elems: self
									.exported_idents
									.iter()
									.map(|e| {
										Some(ExprOrSpread {
											spread: None,
											expr: Box::new(Expr::Ident(Ident::new(
												e.0 .0.clone(),
												DUMMY_SP.with_ctxt(e.0 .1),
											))),
										})
									})
									.collect(),
							})),
						}],
						type_args: None,
					})),
				})));

				// Append annotations to the end of the file.
				new.extend(self.annotations.drain(..).map(ModuleItem::Stmt));
			}
		}

		if self.has_action {
			let mut actions = self.export_actions.clone();

			// All exported values are considered as actions if the file is an action file.
			if self.in_action_file {
				actions.extend(self.exported_idents.iter().map(|e| e.1.clone()));
			};

			let actions = actions
				.into_iter()
				.map(|name| (generate_action_id(&self.file_name, &name), name))
				.collect::<ActionsMap>();
			// Prepend a special comment to the top of the file.
			self.comments.add_leading(
				self.start_pos,
				Comment {
					span: DUMMY_SP,
					kind: CommentKind::Block,
					text: generate_server_actions_comment(actions).into(),
				},
			);

			if self.config.is_react_server_layer {
				// Inlined actions are only allowed on the server layer.
				// import { registerServerReference } from 'private-next-rsc-server-reference'
				// registerServerReference("action_id")
				new.push(ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
					span: DUMMY_SP,
					specifiers: vec![ImportSpecifier::Named(ImportNamedSpecifier {
						span: DUMMY_SP,
						local: quote_ident!("registerServerReference"),
						imported: None,
						is_type_only: false,
					})],
					src: Box::new(Str {
						span: DUMMY_SP,
						value: "swc-plugin-react-server/rsc-server-reference".into(),
						raw: None,
					}),
					type_only: false,
					with: None,
					phase: Default::default(),
				})));

				// Encryption and decryption only happens on the server layer.
				// import { encryptActionBoundArgs, decryptActionBoundArgs } from
				// 'private-next-rsc-action-encryption'
				new.push(ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
					span: DUMMY_SP,
					specifiers: vec![
						ImportSpecifier::Named(ImportNamedSpecifier {
							span: DUMMY_SP,
							local: quote_ident!("encryptActionBoundArgs"),
							imported: None,
							is_type_only: false,
						}),
						ImportSpecifier::Named(ImportNamedSpecifier {
							span: DUMMY_SP,
							local: quote_ident!("decryptActionBoundArgs"),
							imported: None,
							is_type_only: false,
						}),
					],
					src: Box::new(Str {
						span: DUMMY_SP,
						value: "swc-plugin-react-server/rsc-action-encryption".into(),
						raw: None,
					}),
					type_only: false,
					with: None,
					phase: Default::default(),
				})));

				// Make it the first item
				new.rotate_right(2);
			} else {
				// Make it the first item
				new.rotate_right(1);
			}
		}

		*stmts = new;

		self.annotations = old_annotations;
	}

	fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
		let old_annotations = self.annotations.take();

		let mut new = Vec::with_capacity(stmts.len());
		for mut stmt in stmts.take() {
			stmt.visit_mut_with(self);

			new.push(stmt);
			new.append(&mut self.annotations);
		}

		*stmts = new;

		self.annotations = old_annotations;
	}

	noop_visit_mut_type!();
}

fn retain_names_from_declared_idents(child_names: &mut Vec<Name>, current_declared_idents: &[Id]) {
	// Collect the names to retain in a separate vector
	let mut retained_names = Vec::new();

	for name in child_names.iter() {
		let mut should_retain = true;

		// Merge child_names. For example if both `foo.bar` and `foo.bar.baz` are used,
		// we only need to keep `foo.bar` as it covers the other.

		// Currently this is O(n^2) and we can potentially improve this to O(n log n)
		// by sorting or using a hashset.
		for another_name in child_names.iter() {
			if name != another_name
				&& name.0 == another_name.0
				&& name.1.len() >= another_name.1.len()
			{
				let mut is_prefix = true;
				for i in 0..another_name.1.len() {
					if name.1[i] != another_name.1[i] {
						is_prefix = false;
						break;
					}
				}
				if is_prefix {
					should_retain = false;
					break;
				}
			}
		}

		if should_retain
			&& current_declared_idents.contains(&name.0)
			&& !retained_names.contains(name)
		{
			retained_names.push(name.clone());
		}
	}

	// Replace the original child_names with the retained names
	*child_names = retained_names;
}

fn gen_ident(cnt: &mut u32) -> JsWord {
	let id: JsWord = format!("$$ACTION_{}", cnt).into();
	*cnt += 1;
	id
}

fn attach_name_to_expr(ident: Ident, expr: Expr, extra_items: &mut Vec<ModuleItem>) -> Expr {
	// Create the variable `var $$ACTION_0;`
	extra_items.push(ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
		span: DUMMY_SP,
		kind: VarDeclKind::Var,
		declare: Default::default(),
		decls: vec![VarDeclarator {
			span: DUMMY_SP,
			name: ident.clone().into(),
			init: None,
			definite: Default::default(),
		}],
	})))));

	if let Expr::Paren(_paren) = &expr {
		expr
	} else {
		// Create the assignment `($$ACTION_0 = arrow)`
		Expr::Paren(ParenExpr {
			span: DUMMY_SP,
			expr: Box::new(Expr::Assign(AssignExpr {
				span: DUMMY_SP,
				left: ident.into(),
				op: op!("="),
				right: Box::new(expr),
			})),
		})
	}
}

fn generate_action_id(file_name: &str, export_name: &str) -> String {
	// Attach a checksum to the action using sha1:
	// $$id = sha1('file_name' + ':' + 'export_name');
	let mut hasher = Sha1::new();
	hasher.update(file_name.as_bytes());
	hasher.update(b":");
	hasher.update(export_name.as_bytes());
	let result = hasher.finalize();

	hex_encode(result)
}

fn annotate_ident_as_action(
	ident: Ident,
	bound: Vec<Option<ExprOrSpread>>,
	file_name: &str,
	export_name: String,
) -> Expr {
	// Add the proxy wrapper call `registerServerReference($$id, $$bound, myAction,
	// maybe_orig_action)`.
	let action_id = generate_action_id(file_name, &export_name);

	let proxy_expr = Expr::Call(CallExpr {
		span: DUMMY_SP,
		callee: quote_ident!("registerServerReference").as_callee(),
		args: vec![
			// $$id
			ExprOrSpread {
				spread: None,
				expr: Box::new(action_id.clone().into()),
			},
			ExprOrSpread {
				spread: None,
				expr: Box::new(Expr::Ident(ident)),
			},
		],
		type_args: Default::default(),
	});

	if bound.is_empty() {
		proxy_expr
	} else {
		// proxy_expr.bind(null, [encryptActionBoundArgs("id", [arg1, ...])])
		Expr::Call(CallExpr {
			span: DUMMY_SP,
			callee: Expr::Member(MemberExpr {
				span: DUMMY_SP,
				obj: Box::new(proxy_expr),
				prop: MemberProp::Ident(quote_ident!("bind")),
			})
				.as_callee(),
			args: vec![
				ExprOrSpread {
					spread: None,
					expr: Box::new(Expr::Lit(Lit::Null(Null { span: DUMMY_SP }))),
				},
				ExprOrSpread {
					spread: None,
					expr: Box::new(Expr::Call(CallExpr {
						span: DUMMY_SP,
						callee: quote_ident!("encryptActionBoundArgs").as_callee(),
						args: vec![
							ExprOrSpread {
								spread: None,
								expr: Box::new(action_id.into()),
							},
							ExprOrSpread {
								spread: None,
								expr: Box::new(Expr::Array(ArrayLit {
									span: DUMMY_SP,
									elems: bound,
								})),
							},
						],
						type_args: None,
					})),
				},
			],
			type_args: Default::default(),
		})
	}
}

const DIRECTIVE_TYPOS: &[&str] = &[
	"use servers",
	"use-server",
	"use sevrer",
	"use srever",
	"use servre",
	"user server",
];

fn remove_server_directive_index_in_module(
	stmts: &mut Vec<ModuleItem>,
	in_action_file: &mut bool,
	has_action: &mut bool,
	enabled: bool,
) {
	let mut is_directive = true;

	stmts.retain(|stmt| {
		match stmt {
			ModuleItem::Stmt(Stmt::Expr(ExprStmt {
				                            expr: box Expr::Lit(Lit::Str(Str { value, span, .. })),
				                            ..
			                            })) => {
				if value == "use server" {
					if is_directive {
						*in_action_file = true;
						*has_action = true;
						if !enabled {
							HANDLER.with(|handler| {
								handler
									.struct_span_err(
										*span,
										"To use Server Actions, please enable the feature flag in your Next.js config. Read more: https://nextjs.org/docs/app/building-your-application/data-fetching/forms-and-mutations#convention",
									)
									.emit()
							});
						}
						return false;
					} else {
						HANDLER.with(|handler| {
							handler
								.struct_span_err(
									*span,
									"The \"use server\" directive must be at the top of the file.",
								)
								.emit();
						});
					}
				} else {
					// Detect typo of "use server"
					if DIRECTIVE_TYPOS.iter().any(|&s| s == value) {
						HANDLER.with(|handler| {
							handler
								.struct_span_err(
									*span,
									format!(
										"Did you mean \"use server\"? \"{}\" is not a supported \
                                         directive name.",
										value
									)
										.as_str(),
								)
								.emit();
						});
					}
				}
			}
			ModuleItem::Stmt(Stmt::Expr(ExprStmt {
				                            expr:
				                            box Expr::Paren(ParenExpr {
					                                            expr: box Expr::Lit(Lit::Str(Str { value, .. })),
					                                            ..
				                                            }),
				                            span,
				                            ..
			                            })) => {
				// Match `("use server")`.
				if value == "use server" || DIRECTIVE_TYPOS.iter().any(|&s| s == value) {
					if is_directive {
						HANDLER.with(|handler| {
							handler
								.struct_span_err(
									*span,
									"The \"use server\" directive cannot be wrapped in \
                                     parentheses.",
								)
								.emit();
						})
					} else {
						HANDLER.with(|handler| {
							handler
								.struct_span_err(
									*span,
									"The \"use server\" directive must be at the top of the file, \
                                     and cannot be wrapped in parentheses.",
								)
								.emit();
						})
					}
				}
			}
			_ => {
				is_directive = false;
			}
		}
		true
	});
}

fn remove_server_directive_index_in_fn(
	stmts: &mut Vec<Stmt>,
	remove_directive: bool,
	is_action_fn: &mut bool,
	action_span: &mut Option<Span>,
	enabled: bool,
) {
	let mut is_directive = true;

	stmts.retain(|stmt| {
		if let Stmt::Expr(ExprStmt {
			                  expr: box Expr::Lit(Lit::Str(Str { value, span, .. })),
			                  ..
		                  }) = stmt
		{
			if value == "use server" {
				*action_span = Some(*span);

				if is_directive {
					*is_action_fn = true;
					if !enabled {
						HANDLER.with(|handler| {
							handler
								.struct_span_err(
									*span,
									"To use Server Actions, please enable the feature flag in your Next.js config. Read more: https://nextjs.org/docs/app/building-your-application/data-fetching/forms-and-mutations#convention",
								)
								.emit()
						});
					}
					if remove_directive {
						return false;
					}
				} else {
					HANDLER.with(|handler| {
						handler
							.struct_span_err(
								*span,
								"The \"use server\" directive must be at the top of the function \
                                 body.",
							)
							.emit();
					});
				}
			} else {
				// Detect typo of "use server"
				if DIRECTIVE_TYPOS.iter().any(|&s| s == value) {
					HANDLER.with(|handler| {
						handler
							.struct_span_err(
								*span,
								format!(
									"Did you mean \"use server\"? \"{}\" is not a supported \
                                     directive name.",
									value
								)
									.as_str(),
							)
							.emit();
					});
				}
			}
		} else {
			is_directive = false;
		}
		true
	});
}

fn collect_idents_in_array_pat(elems: &[Option<Pat>], ids: &mut Vec<Id>) {
	for elem in elems.iter().flatten() {
		match elem {
			Pat::Ident(ident) => {
				ids.push(ident.id.to_id());
			}
			Pat::Array(array) => {
				collect_idents_in_array_pat(&array.elems, ids);
			}
			Pat::Object(object) => {
				collect_idents_in_object_pat(&object.props, ids);
			}
			Pat::Rest(rest) => {
				if let Pat::Ident(ident) = &*rest.arg {
					ids.push(ident.id.to_id());
				}
			}
			Pat::Assign(AssignPat { left, .. }) => {
				collect_idents_in_pat(left, ids);
			}
			Pat::Expr(..) | Pat::Invalid(..) => {}
		}
	}
}

fn collect_idents_in_object_pat(props: &[ObjectPatProp], ids: &mut Vec<Id>) {
	for prop in props {
		match prop {
			ObjectPatProp::KeyValue(KeyValuePatProp { key, value }) => {
				if let PropName::Ident(ident) = key {
					ids.push(ident.to_id());
				}

				match &**value {
					Pat::Ident(ident) => {
						ids.push(ident.id.to_id());
					}
					Pat::Array(array) => {
						collect_idents_in_array_pat(&array.elems, ids);
					}
					Pat::Object(object) => {
						collect_idents_in_object_pat(&object.props, ids);
					}
					_ => {}
				}
			}
			ObjectPatProp::Assign(AssignPatProp { key, .. }) => {
				ids.push(key.to_id());
			}
			ObjectPatProp::Rest(RestPat { arg, .. }) => {
				if let Pat::Ident(ident) = &**arg {
					ids.push(ident.id.to_id());
				}
			}
		}
	}
}

fn collect_idents_in_var_decls(decls: &[VarDeclarator], ids: &mut Vec<Id>) {
	for decl in decls {
		collect_idents_in_pat(&decl.name, ids);
	}
}

fn collect_idents_in_pat(pat: &Pat, ids: &mut Vec<Id>) {
	match pat {
		Pat::Ident(ident) => {
			ids.push(ident.id.to_id());
		}
		Pat::Array(array) => {
			collect_idents_in_array_pat(&array.elems, ids);
		}
		Pat::Object(object) => {
			collect_idents_in_object_pat(&object.props, ids);
		}
		Pat::Assign(AssignPat { left, .. }) => {
			collect_idents_in_pat(left, ids);
		}
		Pat::Rest(RestPat { arg, .. }) => {
			if let Pat::Ident(ident) = &**arg {
				ids.push(ident.id.to_id());
			}
		}
		Pat::Expr(..) | Pat::Invalid(..) => {}
	}
}

fn collect_decl_idents_in_stmt(stmt: &Stmt, ids: &mut Vec<Id>) {
	if let Stmt::Decl(Decl::Var(var)) = &stmt {
		collect_idents_in_var_decls(&var.decls, ids);
	}
}

pub(crate) struct ClosureReplacer<'a> {
	used_ids: &'a [Name],
}

impl ClosureReplacer<'_> {
	fn index(&self, e: &Expr) -> Option<usize> {
		let name = Name::try_from(e).ok()?;
		self.used_ids.iter().position(|used_id| *used_id == name)
	}
}

impl VisitMut for ClosureReplacer<'_> {
	fn visit_mut_expr(&mut self, e: &mut Expr) {
		e.visit_mut_children_with(self);

		if let Some(index) = self.index(e) {
			*e = Expr::Ident(Ident::new(
				// $$ACTION_ARG_0
				format!("$$ACTION_ARG_{}", index).into(),
				DUMMY_SP,
			));
		}
	}

	fn visit_mut_prop_or_spread(&mut self, n: &mut PropOrSpread) {
		n.visit_mut_children_with(self);

		if let PropOrSpread::Prop(box Prop::Shorthand(i)) = n {
			let name = Name::from(&*i);
			if let Some(index) = self.used_ids.iter().position(|used_id| *used_id == name) {
				*n = PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
					key: PropName::Ident(i.clone()),
					value: Box::new(Expr::Ident(Ident::new(
						// $$ACTION_ARG_0
						format!("$$ACTION_ARG_{}", index).into(),
						DUMMY_SP,
					))),
				})));
			}
		}
	}

	noop_visit_mut_type!();
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct NamePart {
	prop: JsWord,
	is_member: bool,
	optional: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Name(Id, Vec<NamePart>);

impl From<&'_ Ident> for Name {
	fn from(value: &Ident) -> Self {
		Name(value.to_id(), vec![])
	}
}

impl TryFrom<&'_ Expr> for Name {
	type Error = ();

	fn try_from(value: &Expr) -> Result<Self, Self::Error> {
		match value {
			Expr::Ident(i) => Ok(Name(i.to_id(), vec![])),
			Expr::Member(e) => e.try_into(),
			Expr::OptChain(e) => e.try_into(),
			_ => Err(()),
		}
	}
}

impl TryFrom<&'_ MemberExpr> for Name {
	type Error = ();

	fn try_from(value: &MemberExpr) -> Result<Self, Self::Error> {
		match &value.prop {
			MemberProp::Ident(prop) => {
				let mut obj: Name = value.obj.as_ref().try_into()?;
				obj.1.push(NamePart {
					prop: prop.sym.clone(),
					is_member: true,
					optional: false,
				});
				Ok(obj)
			}
			_ => Err(()),
		}
	}
}

impl TryFrom<&'_ OptChainExpr> for Name {
	type Error = ();

	fn try_from(value: &OptChainExpr) -> Result<Self, Self::Error> {
		match &*value.base {
			OptChainBase::Member(m) => match &m.prop {
				MemberProp::Ident(prop) => {
					let mut obj: Name = m.obj.as_ref().try_into()?;
					obj.1.push(NamePart {
						prop: prop.sym.clone(),
						is_member: false,
						optional: value.optional,
					});
					Ok(obj)
				}
				_ => Err(()),
			},
			OptChainBase::Call(_) => Err(()),
		}
	}
}

impl From<Name> for Box<Expr> {
	fn from(value: Name) -> Self {
		let mut expr = Box::new(Expr::Ident(value.0.into()));

		for NamePart {
			prop,
			is_member,
			optional,
		} in value.1.into_iter()
		{
			if is_member {
				expr = Box::new(Expr::Member(MemberExpr {
					span: DUMMY_SP,
					obj: expr,
					prop: MemberProp::Ident(Ident::new(prop, DUMMY_SP)),
				}));
			} else {
				expr = Box::new(Expr::OptChain(OptChainExpr {
					span: DUMMY_SP,
					base: Box::new(OptChainBase::Member(MemberExpr {
						span: DUMMY_SP,
						obj: expr,
						prop: MemberProp::Ident(Ident::new(prop, DUMMY_SP)),
					})),
					optional,
				}));
			}
		}

		expr
	}
}
