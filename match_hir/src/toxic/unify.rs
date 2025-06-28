use super::variables::IsVariable;
use crate::{Binding, Error, ErrorKind, Visitable};
use rustc_span::DUMMY_SP;
use std::{
    any::{Any, type_name},
    cell::RefCell,
    sync::atomic::{AtomicU64, Ordering},
};
use syn::spanned::Spanned;

trait SynNode: Any + std::fmt::Debug + Spanned {}

struct NodeEntryExit<'ast> {
    producer_id: u64,
    node_id: u64,
    enter: bool,
    type_name: &'static str,
    node: &'ast dyn SynNode,
    span: rustc_span::Span,
}

impl std::fmt::Debug for NodeEntryExit<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}:{}:{:?}: {}({})",
            self.producer_id,
            self.node_id,
            self.span,
            if self.enter { "Enter" } else { "Exit" },
            self.type_name,
        ))
    }
}

pub trait Unify {
    /// Arguments
    ///
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

static PRODUCER_ID: AtomicU64 = AtomicU64::new(0);

fn produce(
    root_hir_span: rustc_span::Span,
    scrutinee: &(impl SynNode + Visitable),
) -> Vec<NodeEntryExit> {
    let root_pm2_span = scrutinee.span();
    let mut visitor = Producer {
        producer_id: PRODUCER_ID.fetch_add(1, Ordering::SeqCst),
        node_id: 0,
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
    let mut visitor = Consumer::new(node_entry_exits);
    pattern.visit(&mut visitor);
    if let Some(error) = dbg!(visitor.error) {
        return Err(error);
    }
    let mut collected_bindings = visitor
        .stack
        .borrow()
        .iter()
        .flat_map(|state| {
            if let Reason::Binding(binding) = state.reason {
                Some(binding)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    assert_eq!(bindings.capacity(), collected_bindings.len());
    *bindings = collected_bindings.split_off(0);
    Ok(())
}

struct Producer<'ast> {
    producer_id: u64,
    node_id: u64,
    root_hir_span: rustc_span::Span,
    root_pm2_span: proc_macro2::Span,
    nodes: Vec<NodeEntryExit<'ast>>,
}

impl Producer<'_> {
    fn next_node_id(&mut self) -> u64 {
        let id = self.node_id;
        self.node_id += 1;
        id
    }
}

#[derive(Debug)]
enum Reason<'unify, 'ast> {
    /// Start of parsing
    Init,
    /// Binding of a pattern variable
    Binding(Binding),
    /// Node entry (i.e., a [`NodeEntryExit`] with [`NodeEntryExit::enter`] set to `true`) that was
    /// skipped in attempt to recover from a parsing failure. The skip is "optimistic" in that
    /// parsing could still fail.
    OptimisticSkip(&'unify NodeEntryExit<'ast>),
}

#[derive(Debug)]
struct State<'unify, 'ast> {
    /// Depth in the pattern's parse tree depth when this state was created. This is needed when we
    /// must backtrack. Unbinding a variable and try to bind it to a non-terminal's child must
    /// be done from the same depth at which the state was created originally.
    depth: usize,
    /// Pointer into a [`Producer`]'s `nodes` field
    node_entry_exits: &'unify [NodeEntryExit<'ast>],
    /// Number of leaves consumed
    n_leaves: usize,
    /// Reason for this state's creation. One of:
    /// - [`Reason::Init`] to indicate parsing has started
    /// - [`Binding`] that was created when this state was created
    /// - Node entry (i.e., a [`NodeEntryExit`] with [`NodeEntryExit::enter`] set to `true`) that
    ///   was skipped in attempt to recover from a parsing failure
    reason: Reason<'unify, 'ast>,
}

impl<'unify, 'ast> State<'unify, 'ast> {
    fn new(depth: usize, node_entry_exits: &'unify [NodeEntryExit<'ast>]) -> Self {
        Self {
            depth,
            node_entry_exits,
            n_leaves: 0,
            reason: Reason::Init,
        }
    }

    fn new_with_binding(&self, depth: usize, binding: Binding) -> Self {
        Self {
            depth,
            node_entry_exits: self.node_entry_exits,
            n_leaves: self.n_leaves,
            reason: Reason::Binding(binding),
        }
    }
}

#[derive(Debug)]
struct Consumer<'unify, 'ast> {
    /// [`Consumer`]'s current depth in the pattern's parse tree
    depth: usize,
    /// Stack of [`State`]s. If parsing is successful, the number of bindings in the stack should
    /// equal the number of expected bindings.
    stack: RefCell<Vec<State<'unify, 'ast>>>,
    /// Whether an error has occurred, and if so, which one.
    ///
    /// [`ErrorKind::NoMatch`] is a special case, because it means we must backtrack. This happens
    /// when a variable was bound to too much, for example. In such a case, the [`Consumer`]
    /// should:
    ///
    /// - Pop the stack until a variable that can be unbound is found.
    /// - Clear the error.
    /// - Call [`Visitable::visit_children`] and allow the first child to try to bind the variable.
    ///
    /// If no un-bindable variable can be found, parsing fails.
    ///
    /// If the error is of a kind other than [`ErrorKind::NoMatch`], parsing fails.
    error: Option<Error>,
}

impl<'ast> Producer<'ast> {
    fn visit_inner<T: SynNode + Visitable>(&mut self, node: &'ast T) {
        let node_entry_id = self.next_node_id();
        let type_name = type_name::<T>();
        let node_pm2_span = node.span();
        let node_hir_span = shrink_hir_span(self.root_hir_span, self.root_pm2_span, node_pm2_span);
        self.nodes.push(NodeEntryExit {
            producer_id: self.producer_id,
            node_id: node_entry_id,
            enter: true,
            type_name,
            node,
            span: node_hir_span,
        });
        T::visit_children(node, self);
        let node_exit_id = self.next_node_id();
        self.nodes.push(NodeEntryExit {
            producer_id: self.producer_id,
            node_id: node_exit_id,
            enter: false,
            type_name,
            node,
            span: node_hir_span,
        });
    }
}

impl<'unify, 'ast> Consumer<'unify, 'ast> {
    fn new(node_entry_exits: &'unify [NodeEntryExit<'ast>]) -> Consumer<'unify, 'ast> {
        Self {
            depth: 0,
            stack: RefCell::new(vec![State::new(0, node_entry_exits)]),
            error: None,
        }
    }

    // smoelius: Note that `Consumer::visit_inner` is called on the pattern's nodes, not the HIR's
    // nodes. The `Consumer` refers to the HIR's nodes through its `node_entry_exits` field.
    fn visit_inner<T: std::fmt::Debug + IsVariable + PartialEq + Spanned + Visitable + 'static>(
        &mut self,
        pattern: &'ast T,
    ) {
        // smoelius: `self.error` could be `Some(_)` because `visit_inner` was called on a child's
        // sibling.
        if self.error.is_some() {
            return;
        }

        // smoelius: This loop iterates when a variable is bound, a node entry is skipped, or an
        // error occurs.
        while !self.stack.borrow().top().node_entry_exits.is_empty() {
            // smoelius: If `self.error` is `ErrorKind::NoMatch`, we must backtrack. In such a case,
            // we find the topmost state with a bound variable, unbind the variable, and
            // optimistically skip the entry of the node to which that variable was bound.
            let recovering = match &self.error {
                Some(error) if matches!(error.kind(), ErrorKind::NoMatch) => true,
                Some(_) => return,
                None => false,
            };

            let mut stack = self.stack.borrow_mut();

            if recovering {
                while let Some(top) = stack.last_mut() {
                    if top.depth < self.depth {
                        return;
                    }
                    if top.binding.is_some() {
                        top.reset();
                        break;
                    }
                    stack.pop();
                }
                // smoelius: If the stack is empty (i.e., no un-bindable variable was found),
                // recovery failed.
                if stack.is_empty() {
                    return;
                }
                self.error = None;
            }

            let [
                NodeEntryExit {
                    producer_id: _,
                    node_id: _,
                    enter: true,
                    type_name: _,
                    node: scrutinee,
                    span,
                },
                ..,
            ] = stack.top().node_entry_exits
            else {
                // smoelius: Getting here means there are no more nodes to match against. This can
                // happen (say) if a pattern has a leading variable that was bound to too much.
                // dbg!(stack.top().node_entry_exits_curr);
                self.error = Some(Error::new(DUMMY_SP, ErrorKind::NoMatch));
                return;
            };

            skip_front(&mut stack.top_mut().node_entry_exits, 1);

            // smoelius: When `is_variable` returns true, we are faced with a choice: should we bind
            // the variable or not? The default choice is to bind the variable. If parsing
            // subsequently fails and we must backtrack, the state is popped, the variable is
            // unbound, and the next node entry is skipped.
            if !recovering && pattern.is_variable() {
                dbg!();
                let binding = Binding::new::<T>(*span);
                let state = stack.top().new_with_binding(self.depth, binding);
                stack.push(state);
                skip_up_to_n_for_node_exit(
                    &mut stack.top_mut().node_entry_exits,
                    usize::MAX,
                    *scrutinee,
                );
                // smoelius: The pattern is a variable. Nothing would be gained by looping. We
                // return so that the pattern's next sibling can be processed.
                return;
            }

            assert!(self.error.is_none());

            // smoelius: This is where we recurse on the pattern's children.
            let n_leaves_before = stack.top().n_leaves;
            drop(stack);
            self.depth += 1;
            T::visit_children(pattern, self);
            self.depth -= 1;
            let mut stack = self.stack.borrow_mut();
            let n_leaves_after = stack.top().n_leaves;

            // dbg!(n_leaves_before);
            // eprintln!("{pattern:?}");
            // dbg!(n_leaves_after);

            if self.error.is_some() {
                continue;
            }

            // smoelius: If `self.n_leaves` did not change during the call to `T::visit_children`,
            // then `pattern` is a leaf and must match `scrutinee` exactly.
            if dbg!(n_leaves_before) == dbg!(n_leaves_after) {
                stack.top_mut().n_leaves += 1;
                if (*scrutinee as &dyn Any).downcast_ref::<T>() != Some(pattern) {
                    // dbg!((*scrutinee as &dyn Any).downcast_ref::<T>());
                    // dbg!(pattern);
                    self.error = Some(Error::new(*span, ErrorKind::NoMatch));
                    continue;
                }
            }

            skip_up_to_n_for_node_exit(&mut stack.top_mut().node_entry_exits, 1, *scrutinee);

            dbg!();
        }
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

trait Top {
    type Item;
    fn top(&self) -> &Self::Item;
    fn top_mut(&mut self) -> &mut Self::Item;
}

impl<T> Top for Vec<T> {
    type Item = T;
    fn top(&self) -> &Self::Item {
        self.last().unwrap()
    }
    fn top_mut(&mut self) -> &mut Self::Item {
        self.last_mut().unwrap()
    }
}

fn skip_front<T>(queue: &mut &[T], n: usize) {
    *queue = &queue[n..];
}

fn skip_up_to_n_for_node_exit(queue: &mut &[NodeEntryExit], n: usize, scrutinee: &dyn SynNode) {
    let index = queue
        .iter()
        .take(n)
        .position(is_node_exit(scrutinee))
        .unwrap_or_else(|| panic!("failed to find node exit for {scrutinee:?}"));
    *queue = &queue[index + 1..]
}

fn is_node_exit<'ast>(node: &'ast (dyn SynNode)) -> impl Fn(&NodeEntryExit<'ast>) -> bool {
    |node_entry_exit| {
        if let NodeEntryExit {
            producer_id: _,
            node_id: _,
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
