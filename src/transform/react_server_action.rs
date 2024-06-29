use std::cell::Cell;
use std::path::Path;
use napi::{Error, Status};
use oxc_allocator::{Allocator, Box, Vec};
use oxc_ast::ast::{Argument, BindingIdentifier, BindingPattern, BindingPatternKind, CallExpression, Declaration, ExportNamedDeclaration, Expression, ExpressionStatement, IdentifierReference, ImportOrExportKind, Program, Statement, StringLiteral, VariableDeclaration, VariableDeclarationKind, VariableDeclarator};
use oxc_ast::visit::walk_mut;
use oxc_ast::{VisitMut};

use oxc_parser::Parser;
use oxc_codegen::CodeGenerator;
use oxc_span::{Atom, SourceType};

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
		names: vec![],
		comments: vec![],
	};

	let mut program = ret.program;
	rsc.visit_program(&mut program);

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

	names: std::vec::Vec<String>,

	allocator: &'ast Allocator,
	comments: std::vec::Vec<Comment>,
}

impl<'ast> VisitMut<'ast> for ReactServerAction<'ast> {
	fn visit_program(&mut self, program: &mut Program<'ast>) {
		let mut new_stat = Vec::new_in(self.allocator);
		for stmt in &program.body {
			match stmt {
				Statement::FunctionDeclaration(func) => {
					let func_name: &str = func.id.clone().map_or("UNKNOWN_NAME", |id| id.name.as_str());
					let mut is_server_action = false;
					for stmt in &func.body {
						for d in &stmt.directives {
							if &d.expression.value == "use server" {
								is_server_action = true;
							}
						}
					}

					if self.is_server_layer {
						// convert to `registerServerReference(fn, id);`
						if is_server_action {
							if !func.r#async {
								self.comments.push(Comment {
									start: func.span.start,
									end: func.span.end,
									text: "Server actions must be async.".to_string(),
								});
								// throw error;
							}
							let mut vec: Vec<Argument> = Vec::new_in(self.allocator);
							// func_name
							vec.push(
								Argument::Identifier(
									Box::new_in(
										IdentifierReference {
											span: Default::default(),
											name: Atom::from(func_name),
											reference_id: Cell::new(None),
											reference_flag: Default::default(),
										},
										self.allocator,
									)
								)
							);
							// file_name
							vec.push(
								Argument::StringLiteral(
									Box::new_in(
										StringLiteral {
											span: Default::default(),
											value: Atom::from(self.file_name.as_str()),
										},
										self.allocator,
									)
								)
							);

							let mut var_dec = Vec::new_in(self.allocator);
							let name = oxc_allocator::String::from_str_in(
								format!("{}{}", self.action_export_prefix, func_name).as_str(),
								self.allocator,
							);
							self.names.push(String::from(name.as_str()));

							var_dec.push(
								VariableDeclarator {
									span: Default::default(),
									kind: VariableDeclarationKind::Const,
									id: BindingPattern {
										kind: BindingPatternKind::BindingIdentifier(
											Box::new_in(
												BindingIdentifier {
													span: Default::default(),
													name: Atom::from(
														name.into_bump_str(),
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
													arguments: vec,
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
							new_stat.push(
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
															declarations: var_dec,
															declare: false,
														}, self.allocator,
													)
												)
											),
											specifiers: Vec::new_in(self.allocator),
											source: None,
											export_kind: ImportOrExportKind::Value,
											with_clause: None,
										}
										, self.allocator)
								)
							);
						}
					} else {
						// convert to `registerClientReference(fn, id);`
					}
				}
				_ => {}
			}
		}

		for stmt in new_stat {
			program.body.push(stmt);
		}
		walk_mut::walk_program_mut(self, program);
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_react_server_action() {
		let file_path = "./react_server_action.ts".to_string();
		let is_server_layer = true;
		let is_action_file = true;
		let parsed_code = react_server_action_impl(
			r#"
async function test() {
	"use server"
	return 0;
}
"#.to_string(),
			"__waku__server__",
			file_path,
			is_server_layer,
			is_action_file,
		);
		assert_eq!(parsed_code, r#"async function test() {
	'use server';
	return 0;
}
export const __waku__server__test = registerServerReference(test, './react_server_action.ts');
"#)
	}
}