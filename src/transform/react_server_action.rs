use std::cell::Cell;
use std::path::Path;
use napi::{Error, Status};
use oxc_allocator::{Allocator, Box, Vec};
use oxc_ast::ast::{Argument, ArrowFunctionExpression, BindingIdentifier, BindingPattern, BindingPatternKind, CallExpression, Declaration, ExportNamedDeclaration, Expression, Function, IdentifierReference, ImportOrExportKind, ObjectPropertyKind, Statement, StringLiteral, VariableDeclaration, VariableDeclarationKind, VariableDeclarator};
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

	allow_emit_export: bool,

	rsc_count: u32,
	names: std::vec::Vec<String>,

	new_stat: std::vec::Vec<Statement<'ast>>,

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
	fn generate_register_server_reference_call(
		&self,
		arguments: Vec<'ast, Argument<'ast>>,
	) -> Expression<'ast> {
		Expression::CallExpression(
			Box::new_in(
				CallExpression {
					span: Default::default(),
					callee: Expression::Identifier(
						Box::new_in(
							IdentifierReference {
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

	/// emit `export const $PREFIX$id = registerServerReference(original_func_id, fileId, actionId)`
	fn emit_rsc_export(
		&mut self,
		fn_name: &'ast str,
		file_id: &'ast str,
		export_id: &'ast str,
	) {
		let mut arguments: Vec<Argument> = Vec::new_in(self.allocator);
		// func_name
		arguments.push(
			Argument::Identifier(
				Box::new_in(
					IdentifierReference {
						span: Default::default(),
						name: Atom::from(fn_name),
						reference_id: Cell::new(None),
						reference_flag: Default::default(),
					},
					self.allocator,
				)
			)
		);
		// file_id
		arguments.push(
			Argument::StringLiteral(
				Box::new_in(
					StringLiteral {
						span: Default::default(),
						value: Atom::from(file_id),
					},
					self.allocator,
				)
			)
		);
		// export_id
		arguments.push(
			Argument::StringLiteral(
				Box::new_in(
					StringLiteral {
						span: Default::default(),
						value: Atom::from(export_id),
					},
					self.allocator,
				)
			)
		);

		let mut var = Vec::new_in(self.allocator);
		let export_id = self.get_export_id(export_id);
		self.names.push(String::from(export_id));

		var.push(
			VariableDeclarator {
				span: Default::default(),
				kind: VariableDeclarationKind::Const,
				id: BindingPattern {
					kind: BindingPatternKind::BindingIdentifier(
						Box::new_in(
							BindingIdentifier {
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
					self.generate_register_server_reference_call(arguments)
				),
				definite: false,
			}
		);

		// export const ...
		self.new_stat.push(
			Statement::ExportNamedDeclaration(
				Box::new_in(
					ExportNamedDeclaration {
						span: Default::default(),
						declaration: Some(
							Declaration::VariableDeclaration(
								Box::new_in(
									VariableDeclaration {
										span: Default::default(),
										kind: VariableDeclarationKind::Const,
										declarations: var,
										declare: false,
									}, self.allocator,
								)
							)
						),
						specifiers: Vec::new_in(self.allocator),
						source: None,
						export_kind: ImportOrExportKind::Value,
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
		original_func: &Function<'ast>,
	) -> &'ast str {
		let mut var = Vec::new_in(self.allocator);
		let (file_id, fn_id) = self.generate_action_id(self.file_name.as_str(), original_func.span.start);
		self.rsc_count += 1;
		let mut arguments = Vec::new_in(self.allocator);

		let func: Box<Function<'ast>> = Box::new_in(
			Function::new(
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
			Argument::FunctionExpression(
				func
			)
		);
		// file_id
		arguments.push(
			Argument::StringLiteral(
				Box::new_in(
					StringLiteral {
						span: Default::default(),
						value: Atom::from(file_id),
					},
					self.allocator,
				)
			)
		);
		// export_id
		arguments.push(
			Argument::StringLiteral(
				Box::new_in(
					StringLiteral {
						span: Default::default(),
						value: Atom::from(fn_id),
					},
					self.allocator,
				)
			)
		);

		let export_id = self.get_export_id(fn_id);

		var.push(
			VariableDeclarator {
				span: Default::default(),
				kind: VariableDeclarationKind::Const,
				id: BindingPattern {
					kind: BindingPatternKind::BindingIdentifier(
						Box::new_in(
							BindingIdentifier {
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
					self.generate_register_server_reference_call(arguments)
				),
				definite: false,
			}
		);
		// export const SERVER_ACTION_ID = registerServerReference(func, fileId, actionId);
		self.new_stat.push(
			Statement::ExportNamedDeclaration(
				Box::new_in(
					ExportNamedDeclaration {
						span: Default::default(),
						declaration: Some(
							Declaration::VariableDeclaration(
								Box::new_in(
									VariableDeclaration {
										span: Default::default(),
										kind: VariableDeclarationKind::Const,
										declarations: var,
										declare: false,
									}, self.allocator,
								)
							)
						),
						specifiers: Vec::new_in(self.allocator),
						source: None,
						export_kind: ImportOrExportKind::Value,
						with_clause: None,
					}, self.allocator,
				)
			)
		);
		return export_id;
	}

	fn try_add_function_as_rsc(
		&mut self,
		func: &mut Function<'ast>,
		file_id: &'ast str,
		export_id: &'ast str,
	) {
		let fn_name = match &func.id {
			Some(id) => Some(id.name.as_str()),
			None => None,
		};
		let mut is_server_action = false;
		for stmt in &func.body {
			for d in &stmt.directives {
				if &d.expression.value == "use server" {
					is_server_action = true;
				}
			}
		}

		if self.is_server_layer {
			// convert to `registerServerReference(fn, file_id, export_id);`
			if is_server_action {
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
			}
		} else {
			// convert to `registerClientReference(fn, file_id, export_id);`
		}
	}
}

impl<'ast> VisitMut<'ast> for ReactServerAction<'ast> {
	fn visit_variable_declarator(&mut self, declarator: &mut VariableDeclarator<'ast>) {
		if let Some(expr) = declarator.init.as_mut() {
			match expr {
				Expression::ArrowFunctionExpression(arrow_func) => {
					let arrow_func: Box<ArrowFunctionExpression<'ast>> = Box::new_in(
						ArrowFunctionExpression::new(
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
					let mut arguments: Vec<Argument<'ast>> = Vec::new_in(self.allocator);
					arguments.push(
						Argument::ArrowFunctionExpression(
							arrow_func
						)
					);
					declarator.init = Some(self.generate_register_server_reference_call(arguments));
					walk_mut::walk_variable_declarator_mut(self, declarator);
					return;
				}
				Expression::FunctionExpression(func) => {
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
					declarator.init = Some(Expression::Identifier(
						Box::new_in(
							IdentifierReference {
								span: Default::default(),
								name: Atom::from(export_id),
								reference_id: Cell::new(None),
								reference_flag: Default::default(),
							},
							self.allocator,
						)
					));
				}
				Expression::ObjectExpression(expr) => {
					for kind in expr.properties.iter_mut() {
						match kind {
							ObjectPropertyKind::ObjectProperty(item) => {
								match &item.value {
									Expression::FunctionExpression(func) => {
										let export_id = self.emit_anonymous_rsc_export(func);
										item.value = Expression::Identifier(
											Box::new_in(
												IdentifierReference {
													span: Default::default(),
													name: Atom::from(export_id),
													reference_id: Cell::new(None),
													reference_flag: Default::default(),
												},
												self.allocator,
											)
										);
									}
									Expression::ArrowFunctionExpression(_) => {}
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

	fn visit_function(&mut self, func: &mut Function<'ast>, flags: Option<ScopeFlags>) {
		// if this function is a top-level function, we could add it as a rsc
		let should_add_func_as_rsc: bool = match func.scope_id.get() {
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
		if should_add_func_as_rsc {
			let (file_id, fn_id) = self.generate_action_id(self.file_name.as_str(), func.span.start);

			self.try_add_function_as_rsc(
				func,
				file_id,
				fn_id,
			);
		}
		walk_mut::walk_function_mut(self, func, flags);
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
	b: function() {
		"use server"
		return 0;
	}
}
"#, r#"const a = {b: __waku__server__0716d9708d321ffb6a00818614779e779925365c};
export const __waku__server__0716d9708d321ffb6a00818614779e779925365c = registerServerReference(function() {
	'use server';
	return 0;
}, '9a024afde04fb48946fa537e9d0b5e8a4bfde606', '0716d9708d321ffb6a00818614779e779925365c');
"#,
		);
	}
}