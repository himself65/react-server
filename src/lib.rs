use swc_core::ecma::{
	ast::Program,
	transforms::testing::test_inline,
	visit::{FoldWith},
};
use swc_core::ecma::ast::{Expr, Lit, Module, ModuleItem, Stmt, Str, ExprStmt};
use swc_core::ecma::visit::Fold;
use swc_core::plugin::{
	plugin_transform, proxies::TransformPluginProgramMetadata,
};

enum Directive {
	Server,
	Client,
}

pub struct ReactServer;

// check if string is a "use server" or "use client", then move string literal to the top of the file
impl Fold for ReactServer {
	fn fold_module(&mut self, mut module: Module) -> Module {
		let mut new_items: Vec<ModuleItem> = vec![];
		let mut directive: Option<Directive> = None;
		// find "use server" or "use client" string literal
		for item in module.body {
			if directive.is_some() {
				new_items.push(item);
				continue;
			}
			match item {
				ModuleItem::Stmt(Stmt::Expr(expr)) =>
					match *expr.expr {
						Expr::Lit(
							Lit::Str(
								Str {
									ref value,
									..
								})
						) => {
							if value == "use server" {
								directive = Some(Directive::Server);
							} else if value == "use client" {
								directive = Some(Directive::Client);
							} else {
								new_items.push(ModuleItem::Stmt(Stmt::Expr(expr)));
							}
						}
						_ => {
							new_items.push(ModuleItem::Stmt(Stmt::Expr(expr)));
						}
					}
				_ => {
					new_items.push(item);
				}
			}
		}
		// push "use server" to the top of the file
		match directive {
			Some(Directive::Server) => {
				new_items.insert(
					0,
					ModuleItem::Stmt(
						Stmt::Expr(
							ExprStmt {
								expr: Box::new(
									Expr::Lit(
										Lit::Str(
											Str {
												span: Default::default(),
												value: "use server".into(),
												raw: None,
											}
										)
									)
								),
								span: Default::default(),
							}
						)
					),
				);
			}
			Some(Directive::Client) => {
				new_items.insert(
					0,
					ModuleItem::Stmt(
						Stmt::Expr(
							ExprStmt {
								expr: Box::new(
									Expr::Lit(
										Lit::Str(
											Str {
												span: Default::default(),
												value: "use client".into(),
												raw: None,
											}
										)
									)
								),
								span: Default::default(),
							}
						)
					),
				);
			}
			_ => {}
		}
		module.body = new_items;
		module
	}
}

#[plugin_transform]
pub fn process_transform(program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
	program.fold_with(&mut ReactServer {})
}

test_inline!(
    Default::default(),
    |_| ReactServer,
    non_directive,
    r#"console.log("transform");"#,
    r#"console.log("transform");"#
);

test_inline!(
    Default::default(),
    |_| ReactServer,
    server_directive,
    r#""use server";
console.log("transform");"#,
    r#""use server";
console.log("transform");"#
);

test_inline!(
		Default::default(),
		|_| ReactServer,
		client_directive,
		r#""use client";
console.log("transform");"#,
		r#""use client";
console.log("transform");"#
);

test_inline!(
	Default::default(),
	|_| ReactServer,
	directive_after_import,
	r#"
import { foo } from "bar";
"use server";
console.log("transform");
"#,
	r#"
"use server";
import { foo } from "bar";
console.log("transform");
"#
);