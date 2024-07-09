use std::cell::Cell;
use std::path::Path;
use napi::{Error, Status};
use oxc_allocator::{Allocator, Box, Vec};
use oxc_ast::ast;
use oxc_ast::visit::{walk_mut};
use oxc_ast::{AstBuilder, VisitMut};
use oxc_semantic::{Semantic, SemanticBuilder};
use oxc_parser::Parser;
use oxc_codegen::CodeGenerator;
use oxc_span::{Atom, SourceType};
use sha1::{Digest, Sha1};
use hex::encode;
use oxc_syntax::scope::{ScopeFlags};
use tokio::fs;
use crate::validate::{validate_string, ValidateResult};

#[napi]
pub async fn react_server_action(
	file_path: String,
	action_export_prefix: String,
	is_server_layer: bool,
) -> napi::Result<String> {
	let path = Path::new(&file_path);
	let source_text = String::from_utf8(
		fs::read(path)
			.await
			.map_err(|e| Error::new(Status::Unknown, format!("Failed to read file: {}", e)))?
	).unwrap();
	Ok(react_server_action_impl(source_text, file_path.as_str(), action_export_prefix, is_server_layer))
}

fn react_server_action_impl(
	source_text: String,
	action_export_prefix: &str,
	file_path: String,
	is_server_layer: bool,
) -> String {
	let path = Path::new(&file_path);
	let source_type = SourceType::from_path(path).unwrap();
	let valid_ret = validate_string(&source_text, source_type, is_server_layer).unwrap();

	let allocator = Allocator::default();
	let ret = Parser::new(&allocator, &source_text, source_type).parse();
	let mut program = ret.program;
	let ast = AstBuilder::new(&allocator);
	let sem_ret = SemanticBuilder::new(&source_text, source_type)
		.with_check_syntax_error(true)
		.with_trivias(ret.trivias).build(
		ast.copy(&&program),
	);

	let mut rsc = ReactServerAction {
		file_name: &file_path,
		action_export_prefix,
		is_server_layer,
		rsc_count: 0,
		allocator: &allocator,
		validate_result: valid_ret,
		allow_emit_export: true,
		semantic: sem_ret.semantic,
		ast,
		names: vec![],
		comments: vec![],
		new_stat: vec![],
	};

	rsc.visit_program(&mut program);

	for stat in rsc.new_stat {
		program.body.push(stat);
	}

	CodeGenerator::new().build(&program).source_text
}

#[napi(object)]
#[derive(Debug)]
struct Comment {
	pub start: u32,
	pub end: u32,
	pub text: String,
}

struct ReactServerAction<'ast> {
	file_name: &'ast String,

	action_export_prefix: &'ast str,

	is_server_layer: bool,

	validate_result: ValidateResult,

	allow_emit_export: bool,

	rsc_count: u32,
	names: std::vec::Vec<String>,

	new_stat: std::vec::Vec<ast::Statement<'ast>>,

	ast: AstBuilder<'ast>,
	semantic: Semantic<'ast>,

	allocator: &'ast Allocator,
	comments: std::vec::Vec<Comment>,
}

impl<'ast> ReactServerAction<'ast> {
	// slightly different logic with next.js
	// next.js uses sha1(file_name#export_name) as id, and store function into a global map,
	// but we use id = sha1('file_name')#sha1('start')
	// then we can use jump table Map<string, Promise<string, Function>> in the JS side.
	fn generate_action_id(
		&self,
		file_name: &str,
		start: u32,
	) -> (&'ast str, &'ast str) {
		let mut hasher = Sha1::new();
		hasher.update(file_name.as_bytes());
		let file_name_result = hasher.finalize();
		let mut hasher = Sha1::new();
		hasher.update(start.to_string().as_bytes());
		let fn_id_result = hasher.finalize();

		(
			oxc_allocator::String::from_str_in(
				encode(file_name_result).as_str(),
				self.allocator,
			).into_bump_str(),
			oxc_allocator::String::from_str_in(
				encode(fn_id_result).as_str(),
				self.allocator,
			).into_bump_str()
		)
	}
}

impl<'ast> ReactServerAction<'ast> {
	fn generate_register_call(
		&self,
		func_name: &'ast str,
		arguments: Vec<'ast, ast::Argument<'ast>>,
	) -> ast::Expression<'ast> {
		ast::Expression::CallExpression(
			Box::new_in(
				ast::CallExpression {
					span: Default::default(),
					callee: ast::Expression::Identifier(
						Box::new_in(
							ast::IdentifierReference {
								span: Default::default(),
								name: Atom::from(func_name),
								reference_id: Cell::new(None),
								reference_flag: Default::default(),
							},
							self.allocator,
						)
					),
					arguments,
					optional: false,
					type_parameters: None,
				},
				self.allocator,
			)
		)
	}

	fn get_export_id(
		&self,
		export_id: &'ast str,
	) -> &'ast str {
		oxc_allocator::String::from_str_in(
			format!("{}{}", self.action_export_prefix, export_id).as_str(),
			self.allocator,
		).into_bump_str()
	}

	/// const fn_name = createServerReference(id, callServerRSC)
	/// callServerRSC is a global variable
	fn generate_rsc_reference_call(
		&mut self,
		fn_name: &'ast str,
		id: &'ast str,
	) -> ast::Declaration<'ast> {
		let mut arguments = Vec::new_in(self.allocator);
		arguments.push(
			ast::Argument::StringLiteral(
				Box::new_in(
					ast::StringLiteral {
						span: Default::default(),
						value: Atom::from(id),
					},
					self.allocator,
				)
			));
		arguments.push(
			ast::Argument::Identifier(
				Box::new_in(
					ast::IdentifierReference {
						span: Default::default(),
						name: Atom::from("callServerRSC"),
						reference_id: Cell::new(None),
						reference_flag: Default::default(),
					},
					self.allocator,
				)
			),
		);
		let mut declarations = Vec::new_in(self.allocator);

		declarations.push(
			ast::VariableDeclarator {
				span: Default::default(),
				kind: ast::VariableDeclarationKind::Const,
				id: ast::BindingPattern {
					kind: ast::BindingPatternKind::BindingIdentifier(
						Box::new_in(
							ast::BindingIdentifier {
								span: Default::default(),
								name: Atom::from(fn_name),
								symbol_id: Cell::new(None),
							},
							self.allocator,
						)
					),
					type_annotation: None,
					optional: false,
				},
				init: Some(
					ast::Expression::CallExpression(
						Box::new_in(
							ast::CallExpression {
								span: Default::default(),
								callee: ast::Expression::Identifier(
									Box::new_in(
										ast::IdentifierReference {
											span: Default::default(),
											name: Atom::from("registerServerReference"),
											reference_id: Cell::new(None),
											reference_flag: Default::default(),
										},
										self.allocator,
									)
								),
								arguments,
								optional: false,
								type_parameters: None,
							},
							self.allocator,
						)
					)
				),
				definite: false,
			}
		);

		ast::Declaration::VariableDeclaration(
			Box::new_in(
				ast::VariableDeclaration {
					span: Default::default(),
					kind: ast::VariableDeclarationKind::Const,
					declarations,
					declare: false,
				},
				self.allocator,
			)
		)
	}

	/// For server side
	/// emit `export const $PREFIX$id = registerServerReference(original_func_id, fileId, actionId)`
	/// For client side
	/// emit `export const name = registerClientReference(error, fileId, actionId)`
	fn emit_rsc_export(
		&mut self,
		fn_name: &'ast str,
		file_id: &'ast str,
		export_id: &'ast str,
	) {
		let mut arguments: Vec<ast::Argument> = Vec::new_in(self.allocator);

		if self.validate_result.is_client() && self.is_server_layer {
			// func_name = invalid_rsc_call
			arguments.push(
				ast::Argument::Identifier(
					Box::new_in(
						ast::IdentifierReference {
							span: Default::default(),
							name: Atom::from("invalid_rsc_call"),
							reference_id: Cell::new(None),
							reference_flag: Default::default(),
						},
						self.allocator,
					)
				)
			);
		} else {
			// func_name
			arguments.push(
				ast::Argument::Identifier(
					Box::new_in(
						ast::IdentifierReference {
							span: Default::default(),
							name: Atom::from(fn_name),
							reference_id: Cell::new(None),
							reference_flag: Default::default(),
						},
						self.allocator,
					)
				)
			);
		}
		// file_id
		arguments.push(
			ast::Argument::StringLiteral(
				Box::new_in(
					ast::StringLiteral {
						span: Default::default(),
						value: Atom::from(file_id),
					},
					self.allocator,
				)
			)
		);
		// export_id
		arguments.push(
			ast::Argument::StringLiteral(
				Box::new_in(
					ast::StringLiteral {
						span: Default::default(),
						value: Atom::from(export_id),
					},
					self.allocator,
				)
			)
		);

		let mut var = Vec::new_in(self.allocator);
		let export_id: &str = match self.validate_result.is_client() {
			true => fn_name,
			false => self.get_export_id(export_id)
		};
		self.names.push(String::from(export_id));

		var.push(
			ast::VariableDeclarator {
				span: Default::default(),
				kind: ast::VariableDeclarationKind::Const,
				id: ast::BindingPattern {
					kind: ast::BindingPatternKind::BindingIdentifier(
						Box::new_in(
							ast::BindingIdentifier {
								span: Default::default(),
								name: Atom::from(export_id),
								symbol_id: Cell::new(None),
							},
							self.allocator,
						)
					),
					type_annotation: None,
					optional: false,
				},
				init: Some(
					self.generate_register_call(
						match self.validate_result.is_client() {
							true => "registerClientReference",
							false => "registerServerReference",
						},
						arguments,
					)
				),
				definite: false,
			}
		);

		// export const ...
		self.new_stat.push(
			ast::Statement::ExportNamedDeclaration(
				Box::new_in(
					ast::ExportNamedDeclaration {
						span: Default::default(),
						declaration: Some(
							ast::Declaration::VariableDeclaration(
								Box::new_in(
									ast::VariableDeclaration {
										span: Default::default(),
										kind: ast::VariableDeclarationKind::Const,
										declarations: var,
										declare: false,
									}, self.allocator,
								)
							)
						),
						specifiers: Vec::new_in(self.allocator),
						source: None,
						export_kind: ast::ImportOrExportKind::Value,
						with_clause: None,
					}, self.allocator)
			)
		);
	}

	/// emit `export const SERVER_ACTION_ID = registerServerReference(original_func with impl, fileId, actionId)`
	///
	/// Return the SERVER_ACTION_ID string
	fn emit_anonymous_rsc_export(
		&mut self,
		original_func: &ast::Function<'ast>,
	) -> &'ast str {
		let mut var = Vec::new_in(self.allocator);
		let (file_id, fn_id) = self.generate_action_id(self.file_name.as_str(), original_func.span.start);
		self.rsc_count += 1;
		let mut arguments = Vec::new_in(self.allocator);

		let func: Box<ast::Function<'ast>> = Box::new_in(
			ast::Function::new(
				original_func.r#type,
				original_func.span,
				self.ast.copy(&original_func.id),
				original_func.generator,
				original_func.r#async,
				original_func.declare,
				self.ast.copy(&original_func.this_param),
				self.ast.copy(&original_func.params),
				self.ast.copy(&original_func.body),
				self.ast.copy(&original_func.type_parameters),
				self.ast.copy(&original_func.return_type),
			),
			self.allocator,
		);
		// original function
		arguments.push(
			ast::Argument::FunctionExpression(
				func
			)
		);
		// file_id
		arguments.push(
			ast::Argument::StringLiteral(
				Box::new_in(
					ast::StringLiteral {
						span: Default::default(),
						value: Atom::from(file_id),
					},
					self.allocator,
				)
			)
		);
		// export_id
		arguments.push(
			ast::Argument::StringLiteral(
				Box::new_in(
					ast::StringLiteral {
						span: Default::default(),
						value: Atom::from(fn_id),
					},
					self.allocator,
				)
			)
		);

		let export_id = self.get_export_id(fn_id);

		var.push(
			ast::VariableDeclarator {
				span: Default::default(),
				kind: ast::VariableDeclarationKind::Const,
				id: ast::BindingPattern {
					kind: ast::BindingPatternKind::BindingIdentifier(
						Box::new_in(
							ast::BindingIdentifier {
								span: Default::default(),
								name: Atom::from(export_id),
								symbol_id: Cell::new(None),
							},
							self.allocator,
						)
					),
					type_annotation: None,
					optional: false,
				},
				init: Some(
					self.generate_register_call("registerServerReference", arguments)
				),
				definite: false,
			}
		);
		// export const SERVER_ACTION_ID = registerServerReference(func, fileId, actionId);
		self.new_stat.push(
			ast::Statement::ExportNamedDeclaration(
				Box::new_in(
					ast::ExportNamedDeclaration {
						span: Default::default(),
						declaration: Some(
							ast::Declaration::VariableDeclaration(
								Box::new_in(
									ast::VariableDeclaration {
										span: Default::default(),
										kind: ast::VariableDeclarationKind::Const,
										declarations: var,
										declare: false,
									}, self.allocator,
								)
							)
						),
						specifiers: Vec::new_in(self.allocator),
						source: None,
						export_kind: ast::ImportOrExportKind::Value,
						with_clause: None,
					}, self.allocator,
				)
			)
		);
		return export_id;
	}

	fn is_server_action(
		&mut self,
		func: &ast::Function<'ast>,
	) -> bool {
		let mut has_use_server = false;
		for stmt in &func.body {
			for d in &stmt.directives {
				if &d.expression.value == "use server" {
					has_use_server = true;
				}
			}
		}


		if !self.is_server_layer {
			// client side
			func.r#async && has_use_server
		} else {
			// server side
			func.r#async && (self.is_server_layer || has_use_server)
		}
	}

	fn try_add_function_as_rsc(
		&mut self,
		func: &mut ast::Function<'ast>,
		file_id: &'ast str,
		export_id: &'ast str,
	) {
		let fn_name = match &func.id {
			Some(id) => Some(id.name.as_str()),
			None => None,
		};
		let is_server_action = self.is_server_action(func);
		if self.is_server_layer {
			if self.validate_result.is_client() {
				// do nothing here
			} else if is_server_action {
				// convert to `registerServerReference(fn, file_id, export_id);`
				if !func.r#async {
					return self.comments.push(Comment {
						start: func.span.start,
						end: func.span.end,
						text: "Server actions must be async.".to_string(),
					});
					// throw error;
				}
				match fn_name {
					Some(fn_name) => {
						if self.allow_emit_export {
							self.emit_rsc_export(fn_name, file_id, export_id);
						}
					}
					None => {
						// if this function doesn't have a function name, we should not convert it
					}
				}
			} else {
				// convert to `registerClientReference(() => {
				//  throw new Error();
				// }, file_id, export_id);`
				// handle it in visit_declaration
			}
		} else {
			// convert to `createServerReference(id, callServerRSC);`
			// could not handle here, because Function cannot be converted to CallExpression at this point
			// handle it in visit_declaration
		}
	}
}

/// Case 1: server import client -> registerClientReference
/// Case 2: server import server -> registerServerReference
/// Case 3: client import server -> createServerReference
/// Case 4: client import client -> as-is
impl<'ast> VisitMut<'ast> for ReactServerAction<'ast> {
	fn visit_variable_declarator(&mut self, declarator: &mut ast::VariableDeclarator<'ast>) {
		if let Some(expr) = declarator.init.as_mut() {
			match expr {
				ast::Expression::ArrowFunctionExpression(arrow_func) => {
					let arrow_func: Box<ast::ArrowFunctionExpression<'ast>> = Box::new_in(
						ast::ArrowFunctionExpression::new(
							arrow_func.span,
							arrow_func.expression,
							arrow_func.r#async,
							self.ast.copy(&arrow_func.params),
							self.ast.copy(&arrow_func.body),
							self.ast.copy(&arrow_func.type_parameters),
							self.ast.copy(&arrow_func.return_type),
						),
						self.allocator,
					);
					let mut arguments: Vec<ast::Argument<'ast>> = Vec::new_in(self.allocator);
					arguments.push(
						ast::Argument::ArrowFunctionExpression(
							arrow_func
						)
					);
					declarator.init = Some(self.generate_register_call("registerServerReference", arguments));
					walk_mut::walk_variable_declarator_mut(self, declarator);
					return;
				}
				ast::Expression::FunctionExpression(func) => {
					match func.scope_id.get() {
						Some(scope_id) => {
							let bindings = self.semantic.scopes().get_bindings(
								scope_id
							);
							for (name, symbol_id) in bindings.iter() {
								println!("name: {}, symbol_id: {:?}", name, symbol_id);
								// todo
							}
						}
						None => {
							// no scope, no need to bind vars
						}
					}

					let export_id = self.emit_anonymous_rsc_export(func);
					declarator.init = Some(ast::Expression::Identifier(
						Box::new_in(
							ast::IdentifierReference {
								span: Default::default(),
								name: Atom::from(export_id),
								reference_id: Cell::new(None),
								reference_flag: Default::default(),
							},
							self.allocator,
						)
					));
				}
				ast::Expression::ObjectExpression(expr) => {
					for kind in expr.properties.iter_mut() {
						match kind {
							ast::ObjectPropertyKind::ObjectProperty(item) => {
								match &item.value {
									ast::Expression::FunctionExpression(func) => {
										let export_id = self.emit_anonymous_rsc_export(func);
										item.value = ast::Expression::Identifier(
											Box::new_in(
												ast::IdentifierReference {
													span: Default::default(),
													name: Atom::from(export_id),
													reference_id: Cell::new(None),
													reference_flag: Default::default(),
												},
												self.allocator,
											)
										);
									}
									ast::Expression::ArrowFunctionExpression(_) => {}
									_ => {}
								}
							}
							_ => {}
						}
					}
					walk_mut::walk_variable_declarator_mut(self, declarator);
				}
				_ => {}
			}
		}
		self.allow_emit_export = false;
		walk_mut::walk_variable_declarator_mut(self, declarator);
		self.allow_emit_export = true;
	}

	fn visit_function(&mut self, func: &mut ast::Function<'ast>, flags: Option<ScopeFlags>) {
		// if this function is a top-level function, we should add it to rsc here
		let is_top_level_function: bool = match func.scope_id.get() {
			None => false,
			Some(scope_id) => {
				let parent_id = self.semantic.scopes().get_parent_id(scope_id);
				match parent_id {
					Some(parent_id) => {
						self.semantic.scopes().root_scope_id() == parent_id
					}
					None => false
				}
			}
		};
		if is_top_level_function {
			let (file_id, fn_id) = self.generate_action_id(self.file_name.as_str(), func.span.start);

			self.try_add_function_as_rsc(
				func,
				file_id,
				fn_id,
			);
		}
		walk_mut::walk_function_mut(self, func, flags);
	}

	fn visit_expression(&mut self, expr: &mut ast::Expression<'ast>) {
		match expr {
			ast::Expression::ObjectExpression(obj) => {
				for kind in obj.properties.iter_mut() {
					match kind {
						ast::ObjectPropertyKind::ObjectProperty(obj) => {
							match &obj.value {
								ast::Expression::FunctionExpression(func) => {
									let is_server_action = self.is_server_action(func);
									if is_server_action && self.is_server_layer {
										let export_id = self.emit_anonymous_rsc_export(func);
										obj.value = ast::Expression::Identifier(
											Box::new_in(
												ast::IdentifierReference {
													span: Default::default(),
													name: Atom::from(export_id),
													reference_id: Cell::new(None),
													reference_flag: Default::default(),
												},
												self.allocator,
											)
										);
									}
								}
								_ => {}
							}
						}
						_ => {}
					}
				}
			}
			_ => {}
		}
		walk_mut::walk_expression_mut(self, expr);
	}

	fn visit_export_default_declaration(&mut self, decl: &mut ast::ExportDefaultDeclaration<'ast>) {
		if self.validate_result.is_client() && self.is_server_layer {
			// todo: unfinished
			// convert to `registerClientReference(fn, file_id, export_id);`
			*decl = ast::ExportDefaultDeclaration {
				span: Default::default(),
				declaration: ast::ExportDefaultDeclarationKind::FunctionDeclaration(
					Box::new_in(
						ast::Function {
							r#type: ast::FunctionType::FunctionDeclaration,
							span: Default::default(),
							id: Some(ast::BindingIdentifier {
								span: Default::default(),
								name: "invalid_rsc_call_default".into(),
								symbol_id: Cell::new(None),
							}),
							generator: false,
							r#async: false,
							declare: false,
							this_param: None,
							params: Box::new_in(
								ast::FormalParameters {
									span: Default::default(),
									kind: ast::FormalParameterKind::FormalParameter,
									items: Vec::new_in(self.allocator),
									rest: None,
								},
								self.allocator,
							),
							type_parameters: None,
							return_type: None,
							scope_id: Cell::new(None),
							body: None,
						},
						self.allocator,
					)
				),
				exported: ast::ModuleExportName::IdentifierName(ast::IdentifierName {
					span: Default::default(),
					name: "default".into(),
				}),
			}
		}
	}

	fn visit_export_named_declaration(&mut self, export_decl: &mut ast::ExportNamedDeclaration<'ast>) {
		if self.validate_result.is_client() && self.is_server_layer {
			// convert to `registerClientReference(fn, file_id, export_id);`
			let (file_id, fn_id) = self.generate_action_id(self.file_name.as_str(), export_decl.span.start);
			if let Some(decl) = &export_decl.declaration {
				if let Some(id) = decl.id() {
					self.emit_rsc_export(id.name.as_str(), file_id, fn_id);
					*export_decl = ast::ExportNamedDeclaration {
						span: Default::default(),
						declaration: None,
						specifiers: Vec::new_in(self.allocator),
						source: None,
						export_kind: ast::ImportOrExportKind::Value,
						with_clause: None,
					}
				}
			}
		}
		walk_mut::walk_export_named_declaration_mut(self, export_decl);
	}

	fn visit_declaration(&mut self, decl: &mut ast::Declaration<'ast>) {
		match decl {
			ast::Declaration::FunctionDeclaration(func) => {
				let is_server_action = self.is_server_action(func);
				if !self.is_server_layer & is_server_action {
					// convert to `createServerReference(id, callServerRSC);`
					match &func.id {
						Some(id) => {
							let (_, fn_id) = self.generate_action_id(self.file_name.as_str(), func.span.start);
							*decl = self.generate_rsc_reference_call(
								id.name.as_str(),
								self.get_export_id(fn_id),
							);
						}
						None => {}
					}
				} else if self.is_server_layer && self.validate_result.is_client() {
					// convert to `registerClientReference(fn, file_id, export_id);`
					match &func.id {
						Some(_) => {
							let (_, _) = self.generate_action_id(self.file_name.as_str(), func.span.start);
							// self.emit_rsc_export(
							// 	id.name.as_str(),
							// 	self.get_export_id(fn_id),
							// 	fn_id,
							// );
						}
						None => {}
					}
				}
			}
			_ => {}
		}
		walk_mut::walk_declaration_mut(self, decl);
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	fn test_tsx_server_input(
		input: &str,
		expected_output: &str,
	) {
		let parsed_code = react_server_action_impl(
			input.to_string(),
			"__waku__server__",
			TSX_FILE_PATH.into(),
			IS_SERVER_LAYER,
		);
		assert_eq!(parsed_code, expected_output);
	}

	fn test_tsx_client_input(
		input: &str,
		expected_output: &str,
	) {
		let parsed_code = react_server_action_impl(
			input.to_string(),
			"__waku__server__",
			TSX_FILE_PATH.into(),
			NOT_SERVER_LAYER,
		);
		assert_eq!(parsed_code, expected_output);
	}

	fn test_ts_server_input(
		input: &str,
		expected_output: &str,
	) {
		let parsed_code = react_server_action_impl(
			input.to_string(),
			"__waku__server__",
			NORMAL_FILE_PATH.into(),
			IS_SERVER_LAYER,
		);
		assert_eq!(parsed_code, expected_output);
	}

	const NORMAL_FILE_PATH: &str = "./file.ts";
	const TSX_FILE_PATH: &str = "./file.tsx";
	const IS_SERVER_LAYER: bool = true;
	const NOT_SERVER_LAYER: bool = false;

	#[test]
	fn test_vercel_ai_rsc() {
		let input = r#"
const AI = createAI({
	actions: {
		foo: async function() {
			"use server";
			return 0;
		}
	}
});
export { AI };
"#;
		test_ts_server_input(input, r#"const AI = createAI({actions: {foo: __waku__server__92cfceb39d57d914ed8b14d0e37643de0797ae56}});
export { AI };
export const __waku__server__92cfceb39d57d914ed8b14d0e37643de0797ae56 = registerServerReference(async function() {
	'use server';
	return 0;
}, '9a024afde04fb48946fa537e9d0b5e8a4bfde606', '92cfceb39d57d914ed8b14d0e37643de0797ae56');
"#)
	}

	#[test]
	fn test_client_import_rsc() {
		let input = r#"
export async function a() {
	"use server";
	return 0;
}
"#;
		test_tsx_client_input(
			input, r#"export const a = registerServerReference('__waku__server__fe5dbbcea5ce7e2988b8c69bcfdfde8904aabc1f', callServerRSC);
"#,
		);

		test_tsx_server_input(input, r#"export async function a() {
	'use server';
	return 0;
}
export const __waku__server__fe5dbbcea5ce7e2988b8c69bcfdfde8904aabc1f = registerServerReference(a, 'aa08e78087e8703bec46e0df0ebc4c78800fdbaa', 'fe5dbbcea5ce7e2988b8c69bcfdfde8904aabc1f');
"#);

		// 		test_tsx_server_input(r#"
		// "use client"
		// export default async function () {
		// 	return 0;
		// }"#, r#""#);
	}

	#[test]
	fn test_client_import_general_function() {
		test_ts_server_input(r#"
"use client"
export async function foo() {}"#,
		                     r#"'use client';
export {};
export const foo = registerClientReference(invalid_rsc_call, '9a024afde04fb48946fa537e9d0b5e8a4bfde606', 'fa35e192121eabf3dabf9f5ea6abdbcbc107ac3b');
"#);
	}

	#[test]
	fn test_rsc_inside_component() {
		// fixme: this is wrong, nested function should be converted to rsc correctly
		test_tsx_server_input(
			r#"
export default function Home () {
  const ctx = getContext()

  async function a () {
    "use server";
    return ctx.a
  }

  return (
    <Client a={a}/>
  )
}
"#, r#"export default function Home() {
	const ctx = getContext();
	async function a() {
		'use server';
		return ctx.a;
	}
	return <Client a={a}/>;
}"#,
		);
	}

	#[test]
	fn test_react_server_action() {
		// a "use server" function should be converted correctly
		test_ts_server_input(
			r#"
async function test() {
	"use server"
	return 0;
}
"#, r#"async function test() {
	'use server';
	return 0;
}
export const __waku__server__356a192b7913b04c54574d18c28d46e6395428ab = registerServerReference(test, '9a024afde04fb48946fa537e9d0b5e8a4bfde606', '356a192b7913b04c54574d18c28d46e6395428ab');
"#);
		// if it is not declared as "use server", it should not be converted

		test_ts_server_input(
			r#"
async function test() {
	return 0;
}
"#, r#"async function test() {
	return 0;
}
export const __waku__server__356a192b7913b04c54574d18c28d46e6395428ab = registerServerReference(test, '9a024afde04fb48946fa537e9d0b5e8a4bfde606', '356a192b7913b04c54574d18c28d46e6395428ab');
"#);

		// if it's an anonymous function, it should be converted correctly
		test_ts_server_input(
			r#"
const a = function () {
	"use server"
	return 0;
}
"#, r#"const a = __waku__server__17ba0791499db908433b80f37c5fbc89b870084b;
export const __waku__server__17ba0791499db908433b80f37c5fbc89b870084b = registerServerReference(function() {
	'use server';
	return 0;
}, '9a024afde04fb48946fa537e9d0b5e8a4bfde606', '17ba0791499db908433b80f37c5fbc89b870084b');
"#);

		// server action in object
		test_ts_server_input(
			r#"
const a = {
	b: async function() {
		"use server"
		return 0;
	}
}
"#, r#"const a = {b: __waku__server__0716d9708d321ffb6a00818614779e779925365c};
export const __waku__server__0716d9708d321ffb6a00818614779e779925365c = registerServerReference(async function() {
	'use server';
	return 0;
}, '9a024afde04fb48946fa537e9d0b5e8a4bfde606', '0716d9708d321ffb6a00818614779e779925365c');
"#,
		);
	}
}