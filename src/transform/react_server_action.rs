use std::cell::Cell;
use std::path::Path;
use napi::{Error, Status};
use oxc_allocator::{Allocator, Box, Vec};
use oxc_ast::ast::{Argument, ArrowFunctionExpression, BindingIdentifier, BindingPattern, BindingPatternKind, CallExpression, Declaration, ExportNamedDeclaration, Expression, Function, FunctionType, IdentifierReference, ImportOrExportKind, Program, Statement, StringLiteral, VariableDeclaration, VariableDeclarationKind, VariableDeclarator};
use oxc_ast::visit::{walk_mut};
use oxc_ast::{VisitMut};

use oxc_parser::Parser;
use oxc_codegen::CodeGenerator;
use oxc_span::{Atom, SourceType};
use sha1::{Digest, Sha1};
use hex::encode;
use oxc_syntax::scope::ScopeFlags;

use tokio::fs;

#[napi]
pub async fn react_server_action(
	file_path: String,
	action_export_prefix: String,
	is_server_layer: bool,
	is_action_file: bool,
) -> napi::Result<String> {
	let path = Path::new(&file_path);
	let source_text = String::from_utf8(
		fs::read(path)
			.await
			.map_err(|e| Error::new(Status::Unknown, format!("Failed to read file: {}", e)))?
	).unwrap();
	Ok(react_server_action_impl(source_text, file_path.as_str(), action_export_prefix, is_server_layer, is_action_file))
}

fn react_server_action_impl(
	source_text: String,
	action_export_prefix: &str,
	file_path: String,
	is_server_layer: bool,
	is_action_file: bool,
) -> String {
	let path = Path::new(&file_path);
	let source_type = SourceType::from_path(path).unwrap();

	let allocator = Allocator::default();
	let ret = Parser::new(&allocator, &source_text, source_type).parse();

	let mut rsc = ReactServerAction {
		file_name: &file_path,
		action_export_prefix,
		is_server_layer,
		is_action_file,
		allocator: &allocator,
		is_in_program: false,
		names: vec![],
		comments: vec![],
		new_stat: vec![],
	};

	let mut program = ret.program;
	rsc.visit_program(&mut program);

	for stat in rsc.new_stat {
		program.body.push(stat);
	}

	CodeGenerator::new().build(&program).source_text
}

#[derive(Debug)]
struct Comment {
	start: u32,
	end: u32,
	text: String,
}

struct ReactServerAction<'ast> {
	file_name: &'ast String,

	action_export_prefix: &'ast str,

	is_server_layer: bool,
	is_action_file: bool,

	is_in_program: bool,

	names: std::vec::Vec<String>,

	new_stat: std::vec::Vec<Statement<'ast>>,

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
		let export_id_result = hasher.finalize();

		(
			oxc_allocator::String::from_str_in(
				encode(file_name_result).as_str(),
				self.allocator,
			).into_bump_str(),
			oxc_allocator::String::from_str_in(
				encode(export_id_result).as_str(),
				self.allocator,
			).into_bump_str()
		)
	}
}

impl<'ast> ReactServerAction<'ast> {
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
						let export_id = oxc_allocator::String::from_str_in(
							format!("{}{}", self.action_export_prefix, export_id).as_str(),
							self.allocator,
						);
						self.names.push(String::from(export_id.as_str()));

						var.push(
							VariableDeclarator {
								span: Default::default(),
								kind: VariableDeclarationKind::Const,
								id: BindingPattern {
									kind: BindingPatternKind::BindingIdentifier(
										Box::new_in(
											BindingIdentifier {
												span: Default::default(),
												name: Atom::from(
													export_id.into_bump_str(),
												),
												symbol_id: Cell::new(None),
											},
											self.allocator,
										)
									),
									type_annotation: None,
									optional: false,
								},
								init: Some(
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
					// Anonymous Function
					None => {
						// wrap the function into registerServerReference(fn, file_id, start_span);
						// todo: what should I do to wrap the function?
					}
				}
			}
		} else {
			// convert to `registerClientReference(fn, file_id, export_id);`
		}
	}
}

impl<'ast> VisitMut<'ast> for ReactServerAction<'ast> {
	fn visit_program(&mut self, program: &mut Program<'ast>) {
		for stmt in program.body.iter_mut() {
			match stmt {
				// export ...
				Statement::ExportNamedDeclaration(_) => {
					walk_mut::walk_statement_mut(self, stmt);
				}
				// export default ...
				Statement::ExportDefaultDeclaration(_) => {
					walk_mut::walk_statement_mut(self, stmt);
				}
				// function ...
				Statement::FunctionDeclaration(_) => {
					walk_mut::walk_statement_mut(self, stmt);
				}
				_ => {}
			}
		}
	}

	fn visit_function(&mut self, func: &mut Function<'ast>, flags: Option<ScopeFlags>) {
		let (file_id, export_id) = self.generate_action_id(self.file_name.as_str(), func.span.start);
		self.try_add_function_as_rsc(
			func,
			file_id,
			export_id,
		);
		walk_mut::walk_function_mut(self, func, flags);
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	const NORMAL_FILE_PATH: &str = "./file.ts";
	const IS_SERVER_LAYER: bool = true;
	const IS_ACTION_FILE: bool = true;
	const NOT_SERVER_LAYER: bool = false;
	const NOT_ACTION_FILE: bool = false;

	#[test]
	fn test_react_server_action() {
		// a "use server" function should be converted correctly
		let parsed_code = react_server_action_impl(
			r#"
async function test() {
	"use server"
	return 0;
}
"#.to_string(),
			"__waku__server__",
			NORMAL_FILE_PATH.into(),
			IS_SERVER_LAYER,
			IS_ACTION_FILE,
		);
		assert_eq!(parsed_code, r#"async function test() {
	'use server';
	return 0;
}
export const __waku__server__356a192b7913b04c54574d18c28d46e6395428ab = registerServerReference(test, '9a024afde04fb48946fa537e9d0b5e8a4bfde606', '356a192b7913b04c54574d18c28d46e6395428ab');
"#);
		// if it is not declared as "use server", it should not be converted
		let parsed_code = react_server_action_impl(
			r#"
async function test() {
	return 0;
}
"#.to_string(),
			"__waku__server__",
			NORMAL_FILE_PATH.into(),
			IS_SERVER_LAYER,
			IS_ACTION_FILE,
		);
		assert_eq!(parsed_code, r#"async function test() {
	return 0;
}
"#);
	}
}