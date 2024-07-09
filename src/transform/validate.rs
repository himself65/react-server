use std::path::Path;
use napi::{Error, Status};
use oxc_allocator::Allocator;
use oxc_ast::ast;
use oxc_ast::{Visit};
use oxc_ast::visit::walk;
use oxc_parser::Parser;
use oxc_span::SourceType;
use oxc_syntax::scope::ScopeFlags;

#[napi(object)]
pub struct ValidateResult {
	pub file_type: FileType,
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

impl ValidateResult {
	pub(crate) fn is_client(&self) -> bool {
		self.file_type == FileType::Client
	}
}

///
/// Validate a file is a valid Server file or Client File
///
/// @param code: string - the code to validate
///
/// @param file_path: string - the path to the file
///
/// @param is_server_layer: boolean - if the file is in the server layer, enable this in server side rendering
#[napi]
pub fn validate(
	code: String,
	file_path: String,
	is_server_layer: bool,
) -> napi::Result<ValidateResult> {
	let path = Path::new(&file_path);
	let source_type = SourceType::from_path(path).unwrap();
	validate_string(&code, source_type, is_server_layer)
}

pub fn validate_string(
	source_text: &String,
	source_type: SourceType,
	is_server_layer: bool,
) -> napi::Result<ValidateResult> {
	let allocator = Allocator::default();
	let ret = Parser::new(&allocator, source_text, source_type).parse();

	let size = ret.errors.len();
	for error in ret.errors {
		let error = error.with_source_code(source_text.clone());
		println!("{error:?}");
	}

	if size > 0 {
		return Err(Error::new(Status::Unknown, "Failed to parse file"));
	}

	let program = ret.program;

	let mut rsc = ReactServerComponent {
		is_server_layer,

		is_server_action: false,
		is_client_entry: false,
		error: None,
		has_use_client: false,
		has_use_server: false,
		imports: vec![],
		has_async_function: false,
	};

	rsc.visit_program(&program);

	if rsc.error.is_none() {
		if rsc.has_use_server {
			assert_eq!(rsc.has_use_client, false);
		} else if rsc.has_use_client {
			assert_eq!(rsc.has_use_server, false);
		} else {
			assert_eq!(rsc.has_use_server, false);
			assert_eq!(rsc.has_use_client, false);
		}
	}

	let result = ValidateResult {
		file_type: if rsc.has_use_client {
			FileType::Client
		} else if rsc.has_use_server {
			FileType::Server
		} else {
			FileType::Isomorphic
		},
		is_server_action: rsc.is_server_action,
		error: rsc.error,
		imports: rsc.imports,
	};

	Ok(result)
}

#[derive(Debug, PartialEq)]
#[napi(string_enum)]
pub enum FileType {
	Client,
	Server,
	Isomorphic,
}

#[derive(Debug)]
pub struct ReactServerComponent {
	pub is_server_layer: bool,

	// could this file be treated as server action files
	pub is_server_action: bool,
	// is this file a client entry file
	pub is_client_entry: bool,

	pub error: Option<RSCError>,
	pub imports: Vec<ModuleImports>,

	// has "use client" directive on top of the file
	pub has_use_client: bool,
	// has "use server" directive on top of the file
	pub has_use_server: bool,
	pub has_async_function: bool,
}

impl ReactServerComponent {
	fn has_server_directive(&self, function_body: &ast::FunctionBody) -> bool {
		for d in &function_body.directives {
			if &d.expression.value == "use server" {
				return true;
			}
		}
		false
	}
}


impl<'a> Visit<'a> for ReactServerComponent {
	fn visit_program(&mut self, program: &ast::Program<'a>) {
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
			// final check if this file is a server action file
			if !self.has_use_client {
				if !self.is_server_action {
					if self.has_use_server && self.has_async_function {
						self.is_server_action = true;
					} else if !self.has_use_server & !self.has_use_client {
						self.is_server_action = self.has_async_function && self.is_server_layer;
					}
				}
			}
		}
	}

	fn visit_function(&mut self, func: &ast::Function<'a>, flags: Option<ScopeFlags>) {
		let mut is_server_action = false;
		if let Some(body) = &func.body {
			is_server_action = self.has_server_directive(body);
		}
		// check if server action is valid
		if !func.r#async && is_server_action {
			self.error = Some(RSCError::ServerActionMustBeAsync);
			return;
		}

		self.has_async_function |= func.r#async;

		walk::walk_function(self, func, flags);
	}

	fn visit_arrow_expression(&mut self, func: &ast::ArrowFunctionExpression<'a>) {
		let is_server_action = self.has_server_directive(&func.body);

		// check if server action is valid
		if !func.r#async && is_server_action {
			self.error = Some(RSCError::ServerActionMustBeAsync);
			return;
		}

		self.has_async_function |= func.r#async;

		walk::walk_arrow_expression(self, func);
	}

	fn visit_import_declaration(&mut self, decl: &ast::ImportDeclaration<'a>) {
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
		let rsc = validate_string(&source_text.to_string(), SourceType::from_path("test.js").unwrap(), true).unwrap();
		assert_eq!(rsc.is_server_action, false);
		assert_eq!(rsc.file_type, FileType::Server);
		assert_eq!(rsc.error, None);
	}

	#[test]
	fn test_use_client() {
		let source_text = r#"
		"use client"
	"#;
		let rsc = validate_string(&source_text.to_string(), SourceType::from_path("test.js").unwrap(), true).unwrap();
		assert_eq!(rsc.is_server_action, false);
		assert_eq!(rsc.file_type, FileType::Client);
		assert_eq!(rsc.error, None);
	}

	#[test]
	fn test_use_server_and_client() {
		let source_text = r#"
		"use server"
		"use client"
	"#;
		let rsc = validate_string(&source_text.to_string(), SourceType::from_path("test.js").unwrap(), true).unwrap();
		assert_eq!(rsc.error, Some(RSCError::CannotUseBothClientAndServer));
	}

	#[test]
	fn test_use_client_and_server_action() {
		let source_text = r#"
		"use client"

		async function foo() {}
	"#;
		let rsc = validate_string(&source_text.to_string(), SourceType::from_path("test.js").unwrap(), true).unwrap();
		assert_eq!(rsc.is_server_action, false);
		assert_eq!(rsc.file_type, FileType::Client);
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
		let rsc = validate_string(&source_text.to_string(), SourceType::from_path("test.js").unwrap(), true).unwrap();
		// cannot tell if 'foo' is a server action, depends on if there's a Server component that uses it
		assert_eq!(rsc.is_server_action, true);
		assert_eq!(rsc.file_type, FileType::Server);
		assert_eq!(rsc.error, None);
	}

	#[test]
	fn test_arrow_function() {
		let source_text = r#"
		"use server"

		const foo = async () => {
			"use server"
		}
	"#;
		let rsc = validate_string(&source_text.to_string(), SourceType::from_path("test.js").unwrap(), true).unwrap();
		assert_eq!(rsc.is_server_action, true);
		assert_eq!(rsc.file_type, FileType::Server);
		assert_eq!(rsc.error, None);
	}

	#[test]
	fn client_side_isomorphic() {
		let source_text = r#"
		export async function foo() {
			// unknown if this is a server action
		}
	"#;
		let rsc = validate_string(&source_text.to_string(), SourceType::from_path("test.js").unwrap(), false).unwrap();
		assert_eq!(rsc.is_server_action, false);
		assert_eq!(rsc.file_type, FileType::Isomorphic);
		assert_eq!(rsc.error, None);
	}

	#[test]
	fn server_side_isomorphic() {
		let source_text = r#"
		export async function foo() {
			// unknown if this is a server action
		}
	"#;
		let rsc = validate_string(&source_text.to_string(), SourceType::from_path("test.js").unwrap(), true).unwrap();
		assert_eq!(rsc.is_server_action, true);
		assert_eq!(rsc.file_type, FileType::Isomorphic);
		assert_eq!(rsc.error, None);
	}

	#[test]
	fn server_side_nothing() {
		let source_text = r#"
		// do nothing
	"#;
		let rsc = validate_string(&source_text.to_string(), SourceType::from_path("test.ts").unwrap(), true).unwrap();
		assert_eq!(rsc.is_server_action, false);
		assert_eq!(rsc.file_type, FileType::Isomorphic);
		assert_eq!(rsc.error, None);
	}
}
