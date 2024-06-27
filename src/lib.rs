#![deny(clippy::all)]

#[macro_use]
extern crate napi_derive;

use std::cmp::PartialEq;
use napi::bindgen_prelude::*;
use tokio::fs;

use std::path::Path;
use oxc_allocator::Allocator;
use oxc_ast::{ast::{Function}, visit::walk, Visit};
use oxc_ast::ast::{Directive, ImportDeclaration};
use oxc_parser::Parser;
use oxc_span::{SourceType};
use oxc_syntax::scope::ScopeFlags;

#[napi]
pub async fn validate(
	file_path: String,
) -> Result<ReactServerComponent> {
	let path = Path::new(&file_path);
	let source_text = String::from_utf8(
		fs::read(path)
			.await
			.map_err(|e| Error::new(Status::Unknown, format!("Failed to read file: {}", e)))?
	).unwrap();
	let source_type = SourceType::from_path(path).unwrap();
	validate_string(source_text, source_type)
}

pub fn validate_string(
	source_text: String,
	source_type: SourceType,
) -> Result<ReactServerComponent> {
	let allocator = Allocator::default();
	let ret = Parser::new(&allocator, &source_text, source_type).parse();

	for error in ret.errors {
		let error = error.with_source_code(source_text.clone());
		println!("{error:?}");
	}

	let program = ret.program;

	let mut rsc = ReactServerComponent::default();
	rsc.visit_program(&program);

	Ok(rsc)
}

#[napi]
#[derive(Debug, PartialEq)]
pub enum FileType {
	ServerComponent,
	ServerAction,
	Client,
	Isomorphic,
}

#[napi(object)]
#[derive(Debug)]
pub struct ReactServerComponent {
	pub file_type: FileType,
	pub error: Option<String>,
}

impl Default for ReactServerComponent {
	fn default() -> Self {
		ReactServerComponent {
			file_type: FileType::Isomorphic,
			error: None,
		}
	}
}

impl<'a> Visit<'a> for ReactServerComponent {
	fn visit_directive(&mut self, directive: &Directive<'a>) {
		match directive.expression.value.as_str() {
			"use client" => {
				if self.file_type == FileType::ServerComponent ||
					self.file_type == FileType::ServerAction {
					self.error = Some("Cannot use both client and server".to_string());
				}
				self.file_type = FileType::Client
			}
			"use server" => {
				if self.file_type == FileType::Client {
					self.error = Some("Cannot use both client and server".to_string());
				}
				self.file_type = FileType::ServerComponent
			}
			_ => {}
		}
		walk::walk_directive(self, directive);
	}

	fn visit_import_declaration(&mut self, decl: &ImportDeclaration<'a>) {
		if decl.source.value == "client-only" {
			if self.file_type == FileType::ServerComponent ||
				self.file_type == FileType::ServerAction {
				self.error = Some("Cannot use both client and server".to_string());
			}
			self.file_type = FileType::Client
		} else if decl.source.value == "server-only" {
			if self.file_type == FileType::Client {
				self.error = Some("Cannot use both client and server".to_string());
			}
			self.file_type = FileType::ServerComponent
		}
		walk::walk_import_declaration(self, decl);
	}

	fn visit_function(&mut self, func: &Function<'a>, flags: Option<ScopeFlags>) {
		for stmt in &func.body {
			stmt.directives.iter().for_each(|d| {
				if &d.expression.value == "use server" {
					if self.file_type == FileType::Client {
						panic!("Cannot use both client and server")
					}
					self.file_type = FileType::ServerAction
				}
			});
		}
		walk::walk_function(self, func, flags);
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_use_server() {
		let source_text = r#"
		"use server"
	"#;
		let rsc = validate_string(source_text.to_string(), SourceType::from_path("test.js").unwrap()).unwrap();
		assert_eq!(rsc.file_type, FileType::ServerComponent);
		assert_eq!(rsc.error, None);
	}

	#[test]
	fn test_use_client() {
		let source_text = r#"
		"use client"
	"#;
		let rsc = validate_string(source_text.to_string(), SourceType::from_path("test.js").unwrap()).unwrap();
		assert_eq!(rsc.file_type, FileType::Client);
		assert_eq!(rsc.error, None);
	}

	#[test]
	fn test_use_server_and_client() {
		let source_text = r#"
		"use server"
		"use client"
	"#;
		let rsc = validate_string(source_text.to_string(), SourceType::from_path("test.js").unwrap()).unwrap();
		assert_eq!(rsc.file_type, FileType::Client);
		assert_eq!(rsc.error, Some("Cannot use both client and server".to_string()));
	}
}