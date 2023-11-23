use crate::{CallGraph, FnDecl, Module};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Program {
    pub modules: Vec<Module>,
}

impl Program {
    pub fn fns(&self) -> impl Iterator<Item = &FnDecl> + '_ {
        self.modules.iter().flat_map(|module| module.fns())
    }

    pub fn build_call_graph(&self) -> CallGraph {
        CallGraph::build(self)
    }
}
