use beacon_parser::{AstNode, line_col_to_byte_offset_lossy};
use url::Url;

use super::builder::CfgBuilder;
use super::module::ModuleCFG;

///
/// Builds a complete ModuleCFG including module initialization code and all function CFGs.
pub struct ModuleCFGBuilder<'a> {
    symbol_table: &'a beacon_parser::SymbolTable,
    uri: Url,
    module_name: String,
    source: &'a str,
}

impl<'a> ModuleCFGBuilder<'a> {
    pub fn new(symbol_table: &'a beacon_parser::SymbolTable, uri: Url, module_name: String, source: &'a str) -> Self {
        Self { symbol_table, uri, module_name, source }
    }

    /// Build a ModuleCFG from an AST
    ///
    /// Constructs CFGs for the module initialization code and all function definitions,  resolving call sites using the symbol table.
    pub fn build(&self, ast: &AstNode) -> ModuleCFG {
        let mut module_cfg = ModuleCFG::new(self.uri.clone(), self.module_name.clone());

        if let AstNode::Module { body, .. } = ast {
            let mut init_builder = CfgBuilder::new();
            init_builder.build_module(body);
            let (init_cfg, init_call_sites) =
                init_builder.build_with_resolution(self.symbol_table, &self.uri, self.symbol_table.root_scope);
            module_cfg.module_init_cfg = init_cfg;
            for call_site in init_call_sites {
                module_cfg.add_call_site(call_site);
            }

            self.process_module_body(body, &mut module_cfg);
        }

        module_cfg
    }

    fn process_module_body(&self, body: &[AstNode], module_cfg: &mut ModuleCFG) {
        for stmt in body {
            match stmt {
                AstNode::FunctionDef { name, body: func_body, line, col, .. } => {
                    let byte_offset = line_col_to_byte_offset_lossy(self.source, *line, *col);
                    let scope_id = self.symbol_table.find_scope_at_position(byte_offset);

                    let mut builder = CfgBuilder::new();
                    builder.build_function(func_body);
                    let (cfg, call_sites) = builder.build_with_resolution(self.symbol_table, &self.uri, scope_id);

                    module_cfg.add_function_cfg(scope_id, name.clone(), cfg);
                    for call_site in call_sites {
                        module_cfg.add_call_site(call_site);
                    }
                }
                AstNode::ClassDef { body: class_body, .. } => self.process_module_body(class_body, module_cfg),
                _ => {}
            }
        }
    }
}
