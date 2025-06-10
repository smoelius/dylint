use super::variables::IsVariable;
use crate::{Binding, Error, ErrorKind, Visitable};
use std::any::{Any, type_name};
use syn::spanned::Spanned;

trait SynNode: Any + Spanned {}

struct NodeEntryExit<'ast> {
    enter: bool,
    type_name: &'static str,
    node: &'ast dyn SynNode,
    span: rustc_span::Span,
}

impl std::fmt::Debug for NodeEntryExit<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{:?}: {}({})",
            self.span,
            if self.enter { "Enter" } else { "Exit" },
            self.type_name,
        ))
    }
}

pub trait Unify {
    /// Arguments
    /// - `self` is the "scrutinee".
    /// - `hir_span` is used to produce error messages.
    /// - `pattern` is the AST with "holes".
    /// - `bindings` records the subtrees that fill the holes.
    fn unify(
        &self,
        span: rustc_span::Span,
        pattern: &Self,
        bindings: &mut Vec<Binding>,
    ) -> std::result::Result<(), Error>;
}

impl<T: SynNode + Visitable> Unify for T {
    fn unify(
        &self,
        span: rustc_span::Span,
        pattern: &Self,
        bindings: &mut Vec<Binding>,
    ) -> std::result::Result<(), Error> {
        let node_entry_exits = produce(span, self);
        consume(&mut node_entry_exits.as_slice(), pattern, bindings)
    }
}

fn produce(
    root_hir_span: rustc_span::Span,
    scrutinee: &(impl SynNode + Visitable),
) -> Vec<NodeEntryExit> {
    let root_pm2_span = scrutinee.span();
    let mut visitor = Producer {
        root_hir_span,
        root_pm2_span,
        nodes: Vec::new(),
    };
    scrutinee.visit(&mut visitor);
    visitor.nodes
}

fn consume<'unify, 'ast>(
    node_entry_exits: &'unify mut &'unify [NodeEntryExit<'ast>],
    pattern: &'unify (impl SynNode + Visitable),
    bindings: &'ast mut Vec<Binding>,
) -> std::result::Result<(), Error>
where
    'unify: 'ast,
{
    let mut visitor = Consumer {
        node_entry_exits,
        bindings,
        n_leaves: 0,
        error: None,
    };
    pattern.visit(&mut visitor);
    if let Some(error) = visitor.error {
        Err(error)
    } else {
        Ok(())
    }
}

struct Producer<'ast> {
    root_hir_span: rustc_span::Span,
    root_pm2_span: proc_macro2::Span,
    nodes: Vec<NodeEntryExit<'ast>>,
}

struct Consumer<'unify, 'ast> {
    node_entry_exits: &'unify [NodeEntryExit<'ast>],
    bindings: &'ast mut Vec<Binding>,
    n_leaves: usize,
    error: Option<Error>,
}

impl<'ast> Producer<'ast> {
    fn visit_inner<T: SynNode + Visitable>(&mut self, node: &'ast T) {
        let type_name = type_name::<T>();
        let node_pm2_span = node.span();
        let node_hir_span = shrink_hir_span(self.root_hir_span, self.root_pm2_span, node_pm2_span);
        self.nodes.push(NodeEntryExit {
            enter: true,
            type_name,
            node,
            span: node_hir_span,
        });
        T::visit_children(node, self);
        self.nodes.push(NodeEntryExit {
            enter: false,
            type_name,
            node,
            span: node_hir_span,
        });
    }
}

impl<'ast> Consumer<'_, 'ast> {
    fn visit_inner<T: IsVariable + PartialEq + Spanned + Visitable + 'static>(
        &mut self,
        pattern: &'ast T,
    ) {
        assert!(self.error.is_none());

        let [
            NodeEntryExit {
                enter: true,
                type_name: _,
                node: scrutinee,
                span,
            },
            ..,
        ] = self.node_entry_exits
        else {
            panic!(
                "`consume` called not at node entry: {:#?}",
                self.node_entry_exits
            );
        };
        pop_front(&mut self.node_entry_exits, 1);

        if pattern.is_variable() {
            self.bindings.push(Binding::new::<T>(*span));
            // smoelius: If the next `position` fails, the `assert` just after this `if`-`else` will
            // (intentionally) panic.
            if let Some(n) = self
                .node_entry_exits
                .iter()
                .position(is_node_exit(*scrutinee))
            {
                pop_front(&mut self.node_entry_exits, n);
            }
        } else {
            let n_leaves_before = self.n_leaves;
            T::visit_children(pattern, self);
            if self.error.is_some() {
                return;
            }
            let n_leaves_after = self.n_leaves;

            // smoelius: If `self.n_leaves` did not change during the call to `T::visit_children`,
            // then `pattern` is a leaf and must match `scrutinee` exactly.
            if n_leaves_before == n_leaves_after {
                self.n_leaves += 1;
                if (*scrutinee as &dyn Any).downcast_ref::<T>() != Some(pattern) {
                    self.error = Some(Error::new(*span, ErrorKind::NoMatch));
                    return;
                }
            }
        }

        assert!(
            matches!(self.node_entry_exits, [node_entry_exit, ..] if is_node_exit(*scrutinee)(node_entry_exit)),
            "failed to find node exit: {:#?}",
            self.node_entry_exits,
        );
        pop_front(&mut self.node_entry_exits, 1);
    }
}

// smoelius: The `proc_macro2::Span`s that `match_hir` constructs refer to text, not source files.
// Hence, their lines and columns are relative.
pub(crate) fn shrink_hir_span(
    hir_span: rustc_span::Span,
    pm2_span_larger: proc_macro2::Span,
    pm2_span_smaller: proc_macro2::Span,
) -> rustc_span::Span {
    let byte_range_larger = pm2_span_larger.byte_range();
    let byte_range_smaller = pm2_span_smaller.byte_range();

    assert!(byte_range_larger.start <= byte_range_smaller.start);
    assert!(byte_range_smaller.end <= byte_range_larger.end);

    let start_trim = u32::try_from(byte_range_smaller.start - byte_range_larger.start).unwrap();
    let end_trim = u32::try_from(byte_range_larger.end - byte_range_smaller.end).unwrap();

    hir_span
        .with_lo(hir_span.lo() + rustc_span::BytePos(start_trim))
        .with_hi(hir_span.hi() + rustc_span::BytePos(end_trim))
}

fn pop_front<T>(queue: &mut &[T], n: usize) {
    *queue = &queue[n..];
}

fn is_node_exit<'ast>(node: &'ast (dyn SynNode)) -> impl Fn(&NodeEntryExit<'ast>) -> bool {
    |node_entry_exit| {
        if let NodeEntryExit {
            enter: false,
            type_name: _,
            node: other,
            span: _,
        } = node_entry_exit
        {
            std::ptr::from_ref::<dyn SynNode>(node) == *other
        } else {
            false
        }
    }
}

macro_rules! impl_is_variable {
    ($ty:ident) => {
        impl IsVariable for syn::$ty {}
    };
}

macro_rules! impl_syn_node {
    ($ty:ident) => {
        impl SynNode for syn::$ty {}
    };
}

include!(concat!(env!("OUT_DIR"), "/unify_variable.rs"));
include!(concat!(env!("OUT_DIR"), "/unify_node.rs"));
include!(concat!(env!("OUT_DIR"), "/unify_producer.rs"));
include!(concat!(env!("OUT_DIR"), "/unify_consumer.rs"));
