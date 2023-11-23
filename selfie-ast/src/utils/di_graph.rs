use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug)]
pub struct DiGraph<N> {
    nodes: HashSet<N>,
    edges: HashSet<(N, N)>,
    incoming: HashMap<N, HashSet<N>>,
    outgoing: HashMap<N, HashSet<N>>,
}

impl<N> DiGraph<N> {
    pub fn new() -> Self {
        Self {
            nodes: HashSet::new(),
            edges: HashSet::new(),
            incoming: HashMap::new(),
            outgoing: HashMap::new(),
        }
    }
}

impl<N> DiGraph<N>
where
    N: Copy + Eq + Hash,
{
    pub fn add_node(&mut self, node: N) {
        self.nodes.insert(node);
        self.incoming.entry(node).or_default();
        self.outgoing.entry(node).or_default();
    }

    pub fn add_edge(&mut self, a: N, b: N) {
        self.add_node(a);
        self.add_node(b);

        self.edges.insert((a, b));
        self.outgoing.entry(a).or_default().insert(b);
        self.incoming.entry(b).or_default().insert(a);
    }

    pub fn incoming_edges(&self, node: N) -> &HashSet<N> {
        assert!(self.nodes.contains(&node));
        self.incoming.get(&node).expect("node not in graph")
    }

    pub fn outgoing_edges(&self, node: N) -> &HashSet<N> {
        assert!(self.nodes.contains(&node));
        self.outgoing.get(&node).expect("node not in graph")
    }

    pub fn sources(&self) -> impl Iterator<Item = N> + '_ {
        self.nodes
            .iter()
            .filter(|&&node| self.incoming_edges(node).is_empty())
            .copied()
    }

    pub fn sinks(&self) -> impl Iterator<Item = N> + '_ {
        self.nodes
            .iter()
            .filter(|&&node| self.outgoing_edges(node).is_empty())
            .copied()
    }

    pub fn successors(&self, node: N) -> impl Iterator<Item = N> + '_ {
        self.outgoing_edges(node).iter().copied()
    }

    pub fn transitive_successors(&self, node: N) -> impl Iterator<Item = N> + '_ {
        let mut visited = HashSet::new();
        let mut stack: Vec<_> = self.successors(node).collect();

        std::iter::from_fn(move || {
            while let Some(node) = stack.pop() {
                if visited.insert(node) {
                    stack.extend(self.successors(node));
                    return Some(node);
                }
            }

            None
        })
    }

    pub fn predecessors(&self, node: N) -> impl Iterator<Item = N> + '_ {
        self.incoming_edges(node).iter().copied()
    }

    pub fn transitive_predecessors(&self, node: N) -> impl Iterator<Item = N> + '_ {
        let mut visited = HashSet::new();
        let mut stack: Vec<_> = self.predecessors(node).collect();

        std::iter::from_fn(move || {
            while let Some(node) = stack.pop() {
                if visited.insert(node) {
                    stack.extend(self.predecessors(node));
                    return Some(node);
                }
            }

            None
        })
    }

    pub fn strongly_connected_components(&self) -> Vec<Vec<N>> {
        let mut components = Vec::new();
        let mut visited = HashSet::new();

        for node in self.nodes.iter().copied() {
            if visited.insert(node) {
                let mut component = Vec::new();
                let mut stack = vec![node];

                while let Some(node) = stack.pop() {
                    component.push(node);

                    for succ in self.successors(node) {
                        if visited.insert(succ) {
                            stack.push(succ);
                        }
                    }
                }

                components.push(component);
            }
        }

        components
    }

    pub fn topological_sort(&self) -> Vec<N> {
        let mut sorted = Vec::new();
        let mut visited = HashSet::new();

        for node in self.nodes.iter().copied() {
            if visited.insert(node) {
                let mut stack = vec![node];

                while let Some(node) = stack.pop() {
                    let mut all_visited = true;

                    for succ in self.successors(node) {
                        if visited.insert(succ) {
                            stack.push(succ);
                            all_visited = false;
                        }
                    }

                    if all_visited {
                        sorted.push(node);
                    }
                }
            }
        }

        sorted
    }
}

impl<N> Default for DiGraph<N> {
    fn default() -> Self {
        Self::new()
    }
}
