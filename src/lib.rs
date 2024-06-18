#![recursion_limit = "2048"]
#![feature(box_patterns)]
#![feature(arbitrary_self_types)]

use swc_core::common::FileName;
use swc_core::ecma::ast::Program;
use swc_core::ecma::visit::{FoldWith};
use swc_core::plugin::{
	plugin_transform, proxies::TransformPluginProgramMetadata,
};
use crate::transforms::react_actions::{Config, server_actions};

pub mod transforms;

impl Default for Config {
	fn default() -> Self {
		Config {
			is_react_server_layer: true,
			enabled: true,
		}
	}
}

#[plugin_transform]
pub fn process_transform(program: Program, metadata: TransformPluginProgramMetadata) -> Program {
	let plugin_config = metadata.get_transform_plugin_config();
	let config: Config = if let Some(plugin_config) = plugin_config {
		serde_json::from_str(&plugin_config).unwrap_or_else(|f| {
			println!("Could not deserialize instrumentation option");
			println!("{:#?}", f);
			Default::default()
		})
	} else {
		Default::default()
	};
	let mut actions = server_actions(
		match metadata.source_map.source_file.get() {
			Some(file) => &file.name,
			None => &FileName::Anon,
		},
		config,
		metadata.comments.clone(),
	);
	program.fold_with(&mut actions)
}