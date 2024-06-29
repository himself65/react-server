use std::path::Path;
use napi::{Error, Status};
use oxc_allocator::Allocator;
use oxc_ast::ast::{Function, ImportDeclaration, Program};
use oxc_ast::{Trivias, Visit};
use oxc_ast::visit::walk;
use oxc_parser::Parser;
use oxc_span::SourceType;
use oxc_syntax::scope::ScopeFlags;
use tokio::fs;

#[napi(object)]
pub struct ValidateResult {
	pub is_client_entry: bool,
	pub is_server_action: bool,
	pub error: Option<RSCError>,
	pub imports: Vec<ModuleImports>,
}

#[napi]
#[derive(Debug, PartialEq)]
pub enum RSCError {
	CannotUseBothClientAndServer,
	ServerActionMustBeAsync,
}

#[napi(object)]
#[derive(Debug)]
pub struct ModuleImports {
	pub name: String,
}

#[napi]
pub async fn validate(
	file_path: String,
) -> napi::Result<ValidateResult> {
	let path = Path::new(&file_path);
	let source_text = String::from_utf8(
		fs::read(path)
			.await
			.map_err(|e| Error::new(Status::Unknown, format!("Failed to read file: {}", e)))?
	).unwrap();
	let source_type = SourceType::from_path(path).unwrap();
	validate_string(source_text, source_type)
}

fn validate_string(
	source_text: String,
	source_type: SourceType,
) -> napi::Result<ValidateResult> {
	let allocator = Allocator::default();
	let ret = Parser::new(&allocator, &source_text, source_type).parse();

	let size = ret.errors.len();
	for error in ret.errors {
		let error = error.with_source_code(source_text.clone());
		println!("{error:?}");
	}

	if size > 0 {
		return Err(Error::new(Status::Unknown, "Failed to parse file"));
	}

	let program = ret.program;

	let mut rsc = ReactServerComponent::default();
	rsc.visit_program(&program);

	let result = ValidateResult {
		is_client_entry: rsc.has_use_client && !rsc.has_use_server,
		is_server_action: rsc.is_server_action,
		error: rsc.error,
		imports: rsc.imports,
	};

	Ok(result)
}

#[derive(Debug)]
pub struct ReactServerComponent {
	pub is_server_action: bool,
	pub error: Option<RSCError>,
	pub imports: Vec<ModuleImports>,

	// has "use client" directive on top of the file
	has_use_client: bool,
	// has "use server" directive on top of the file
	has_use_server: bool,
}

impl<'a> Default for ReactServerComponent {
	fn default() -> Self {
		ReactServerComponent {
			is_server_action: false,
			error: None,

			has_use_client: false,
			has_use_server: false,

			imports: vec![],
		}
	}
}

impl<'a> Visit<'a> for ReactServerComponent {
	fn visit_program(&mut self, program: &Program<'a>) {
		program.directives.iter().for_each(|d| {
			if &d.expression.value == "use server" {
				self.has_use_server = true;
			} else if &d.expression.value == "use client" {
				self.has_use_client = true;
			}
		});
		if self.has_use_server && self.has_use_client {
			self.error = Some(RSCError::CannotUseBothClientAndServer);
		} else {
			walk::walk_program(self, program);
			if self.has_use_server && self.has_use_client {
				self.error = Some(RSCError::CannotUseBothClientAndServer);
			}
		}
	}

	fn visit_function(&mut self, func: &Function<'a>, flags: Option<ScopeFlags>) {
		let mut is_server_action = false;
		for stmt in &func.body {
			stmt.directives.iter().for_each(|d| {
				if &d.expression.value == "use server" {
					self.is_server_action = true;
					is_server_action = true;
				}
			});
		}
		// check if server action is valid
		if !func.r#async && is_server_action {
			self.error = Some(RSCError::ServerActionMustBeAsync);
			return;
		}

		walk::walk_function(self, func, flags);
	}

	fn visit_import_declaration(&mut self, decl: &ImportDeclaration<'a>) {
		if decl.source.value == "client-only" {
			self.has_use_client = true;
		} else if decl.source.value == "server-only" {
			self.has_use_server = true;
		}
		self.imports.push(
			ModuleImports {
				name: decl.source.value.to_string()
			}
		);
		walk::walk_import_declaration(self, decl);
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
		assert_eq!(rsc.is_server_action, false);
		assert_eq!(rsc.error, None);
	}

	#[test]
	fn test_use_client() {
		let source_text = r#"
		"use client"
	"#;
		let rsc = validate_string(source_text.to_string(), SourceType::from_path("test.js").unwrap()).unwrap();
		assert_eq!(rsc.is_server_action, false);
		assert_eq!(rsc.error, None);
	}

	#[test]
	fn test_use_server_and_client() {
		let source_text = r#"
		"use server"
		"use client"
	"#;
		let rsc = validate_string(source_text.to_string(), SourceType::from_path("test.js").unwrap()).unwrap();
		assert_eq!(rsc.is_server_action, false);
		assert_eq!(rsc.error, Some(RSCError::CannotUseBothClientAndServer));
	}

	#[test]
	fn test_use_client_and_server_action() {
		let source_text = r#"
		"use client"

		async function foo() {
			"use server"
		}
	"#;
		let rsc = validate_string(source_text.to_string(), SourceType::from_path("test.js").unwrap()).unwrap();
		assert_eq!(rsc.is_server_action, true);
		assert_eq!(rsc.error, None);
	}

	#[test]
	fn test_use_server_and_server_action_without_directive() {
		let source_text = r#"
		"use server"

		export async function foo() {
			// no directive
			return 0
		}

		export default function () {
			return 1
		}
	"#;
		let rsc = validate_string(source_text.to_string(), SourceType::from_path("test.js").unwrap()).unwrap();
		// cannot tell if 'foo' is a server action, depends on if there's a Server component that uses it
		assert_eq!(rsc.is_server_action, false);
		assert_eq!(rsc.error, None);
	}
}
