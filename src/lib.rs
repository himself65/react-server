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

enum Detective {
	Server,
	Client,
}

pub struct ReactServer;

// check if string is a "use server" or "use client", then move string literal to the top of the file
impl Fold for ReactServer {
	fn fold_module(&mut self, mut module: Module) -> Module {
		let mut new_items: Vec<ModuleItem> = vec![];
		let mut detective: Option<Detective> = None;
		// find "use server" or "use client" string literal
		for item in module.body {
			if detective.is_some() {
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
								detective = Some(Detective::Server);
							} else if value == "use client" {
								detective = Some(Detective::Client);
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
		match detective {
			Some(Detective::Server) => {
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
			Some(Detective::Client) => {
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
    non_detective,
    r#"console.log("transform");"#,
    r#"console.log("transform");"#
);

test_inline!(
    Default::default(),
    |_| ReactServer,
    server_detective,
    r#""use server";
console.log("transform");"#,
    r#""use server";
console.log("transform");"#
);

test_inline!(
		Default::default(),
		|_| ReactServer,
		client_detective,
		r#""use client";
console.log("transform");"#,
		r#""use client";
console.log("transform");"#
);

test_inline!(
	Default::default(),
	|_| ReactServer,
	detective_after_import,
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