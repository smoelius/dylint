use crate::{span, type_name};
use rustc_hir::{HirId, intravisit::Visitor};
use rustc_lint::LateContext;
use rustc_middle::ty::TyCtxt;
use std::{cell::OnceCell, collections::BTreeMap};

thread_local! {
    // smoelius: The `HirId`s in each `Vec` are ordered "more ancestral" to "less ancestral". In
    // particular, if x is an ancestor of y, x should appear before y in the `Vec`.
    static SPAN_HIR_ID_MAP: OnceCell<BTreeMap<rustc_span::Span, Vec<HirId>>> = const { OnceCell::new() };
}

pub(crate) fn hir_ids_from_span<T>(cx: &LateContext, span: rustc_span::Span) -> Vec<HirId> {
    SPAN_HIR_ID_MAP.with(|map| {
        let map = map.get_or_init(|| init(cx));

        if let Some(hir_ids) = map.get(&span) {
            hir_ids
                .iter()
                .filter(|&&hir_id| {
                    let node = cx.tcx.hir_node(hir_id);
                    type_name(node) == Some(std::any::type_name::<T>())
                })
                .copied()
                .collect()
        } else {
            Vec::default()
        }
    })
}

// https://doc.rust-lang.org/beta/nightly-rustc/rustc_hir/intravisit/index.html
fn init(cx: &LateContext) -> BTreeMap<rustc_span::Span, Vec<HirId>> {
    let mut visitor = SpanHirIdVisitor {
        tcx: cx.tcx,
        map: BTreeMap::default(),
    };
    cx.tcx.hir_visit_all_item_likes_in_crate(&mut visitor);
    visitor.map
}

struct SpanHirIdVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    map: BTreeMap<rustc_span::Span, Vec<HirId>>,
}

impl<'tcx> Visitor<'tcx> for SpanHirIdVisitor<'tcx> {
    type NestedFilter = rustc_middle::hir::nested_filter::OnlyBodies;

    fn maybe_tcx(&mut self) -> Self::MaybeTyCtxt {
        self.tcx
    }

    fn visit_id(&mut self, hir_id: HirId) {
        if let Some(span) = span(self.tcx, hir_id) {
            self.map.entry(span).or_default().push(hir_id);
        }
    }
}
