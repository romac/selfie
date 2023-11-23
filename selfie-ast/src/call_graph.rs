use crate::utils::DiGraph;
use crate::visitor::ExprVisitor;
use crate::{FnCall, MethodCall, Program, Sym};

#[derive(Debug)]
pub struct CallGraph {
    graph: DiGraph<Sym>,
}

impl CallGraph {
    pub fn build(program: &Program) -> Self {
        let mut graph = DiGraph::new();

        for module in &program.modules {
            for fn_decl in module.fns() {
                let caller = fn_decl.sym();
                graph.add_node(caller);

                let mut visitor = CallVisitor::new(caller, &mut graph);
                visitor.visit_expr(&fn_decl.body);
            }
        }

        Self { graph }
    }

    pub fn is_recursive(&self, fn_sym: Sym) -> bool {
        self.graph
            .transitive_successors(fn_sym)
            .any(|sym| sym == fn_sym)
    }

    pub fn is_self_recursive(&self, fn_sym: Sym) -> bool {
        self.graph.successors(fn_sym).any(|sym| sym == fn_sym)
    }

    pub fn callers(&self, fn_sym: Sym) -> impl Iterator<Item = Sym> + '_ {
        self.graph.predecessors(fn_sym)
    }

    pub fn transitive_callers(&self, fn_sym: Sym) -> impl Iterator<Item = Sym> + '_ {
        self.graph.transitive_predecessors(fn_sym)
    }

    pub fn callees(&self, fn_sym: Sym) -> impl Iterator<Item = Sym> + '_ {
        self.graph.successors(fn_sym)
    }

    pub fn transitive_callees(&self, fn_sym: Sym) -> impl Iterator<Item = Sym> + '_ {
        self.graph.transitive_successors(fn_sym)
    }

    pub fn build_fn_info(&self, fn_sym: Sym) -> FnInfo {
        FnInfo {
            sym: fn_sym,
            is_recursive: self.is_recursive(fn_sym),
            is_self_recursive: self.is_self_recursive(fn_sym),
            callers: self.callers(fn_sym).collect(),
            callees: self.callees(fn_sym).collect(),
            transitive_callers: self.transitive_callers(fn_sym).collect(),
            transitive_callees: self.transitive_callees(fn_sym).collect(),
        }
    }
}

#[derive(Debug)]
pub struct FnInfo {
    pub sym: Sym,
    pub is_recursive: bool,
    pub is_self_recursive: bool,
    pub callers: Vec<Sym>,
    pub callees: Vec<Sym>,
    pub transitive_callers: Vec<Sym>,
    pub transitive_callees: Vec<Sym>,
}

struct CallVisitor<'a> {
    caller: Sym,
    graph: &'a mut DiGraph<Sym>,
}

impl<'a> CallVisitor<'a> {
    fn new(caller: Sym, graph: &'a mut DiGraph<Sym>) -> Self {
        Self { caller, graph }
    }
}

impl<'a> ExprVisitor for CallVisitor<'a> {
    fn visit_fn_call(&mut self, call: &FnCall) {
        let callee = call.sym();
        self.graph.add_edge(self.caller, callee);

        self.visit_args(&call.args);
    }

    fn visit_method_call(&mut self, call: &MethodCall) {
        let callee = call.sym();
        self.graph.add_edge(self.caller, callee);

        self.visit_expr(&call.expr);
        self.visit_args(&call.args);
    }
}
