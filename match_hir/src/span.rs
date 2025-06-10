#![allow(clippy::disallowed_methods)]

use rustc_hir::{Arm, Expr, HirId, Node};
use rustc_middle::{dep_graph::DepContext, ty::TyCtxt};

// smoelius: Use of `syn` type names is a convenience. These type names should really be `hir` type
// names.
#[cfg(feature = "debug-spans")]
const HIR_INNER_SPAN_IS_LARGER: &[&str] = &["syn::expr::Arm", "syn::expr::Expr"];

#[must_use]
pub fn span(tcx: TyCtxt, hir_id: HirId) -> Option<rustc_span::Span> {
    let node = tcx.hir_node(hir_id);

    let span = match node {
        Node::Arm(Arm { span, .. }) | Node::Expr(Expr { span, .. }) => {
            #[cfg(feature = "debug-spans")]
            assert!(HIR_INNER_SPAN_IS_LARGER.contains(&crate::type_name(node).unwrap()));
            *span
        }
        _ => tcx.hir().span(hir_id),
    };

    if is_no_location_span(tcx, span) || span.from_expansion() {
        return None;
    }

    #[cfg(feature = "debug-spans")]
    debug_spans(tcx, span, hir_id);

    Some(span)
}

#[cfg(feature = "debug-spans")]
fn debug_spans(tcx: TyCtxt, mut span: rustc_span::Span, hir_id: HirId) {
    if is_no_location_span(tcx, span) || span.from_expansion() {
        return;
    }

    for (parent_hir_id, node) in tcx.hir().parent_iter(hir_id) {
        let parent_span = tcx.hir().span(parent_hir_id);

        if parent_span.from_expansion() {
            continue;
        }

        if !parent_span.contains(span)
            && let Some(type_name) = crate::type_name(node)
            && !HIR_INNER_SPAN_IS_LARGER.contains(&type_name)
        {
            panic!("{type_name} span {parent_span:?} does not contain {span:?}");
        }

        span = parent_span;
    }
}

fn is_no_location_span(tcx: TyCtxt, span: rustc_span::Span) -> bool {
    let (source_file, _lo_line, _lo_col, _hi_line, _hi_col) =
        tcx.sess().source_map().span_to_location_info(span);

    source_file.is_none()
}
