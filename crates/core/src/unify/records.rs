//! Record and row-variable unification logic.

use super::Unifier;
use crate::{ClassRegistry, Result, Subst, Type, TypeVar, TypeVarConstraintRegistry};
use rustc_hash::FxHashSet;

impl Unifier {
    pub(super) fn unify_records(
        fields1: &[(String, Type)], row1: &Option<TypeVar>, fields2: &[(String, Type)], row2: &Option<TypeVar>,
        registry: &TypeVarConstraintRegistry, class_registry: Option<&ClassRegistry>,
    ) -> Result<Subst> {
        let mut subst = Subst::empty();
        let map1: std::collections::HashMap<_, _> = fields1.iter().map(|(k, v)| (k, v)).collect();
        let map2: std::collections::HashMap<_, _> = fields2.iter().map(|(k, v)| (k, v)).collect();
        let mut unified_fields = FxHashSet::default();
        for (name, type1) in &map1 {
            if let Some(type2) = map2.get(name) {
                let s = Self::unify_impl(&subst.apply(type1), &subst.apply(type2), registry, class_registry)?;
                subst = s.compose(subst);
                unified_fields.insert(name);
            }
        }

        let remaining1: Vec<_> = map1.iter().filter(|(name, _)| !unified_fields.contains(name)).collect();
        let remaining2: Vec<_> = map2.iter().filter(|(name, _)| !unified_fields.contains(name)).collect();

        match (remaining1.is_empty(), remaining2.is_empty(), row1, row2) {
            (true, true, None, None) => Ok(subst),
            (false, true, None, Some(rv2)) => {
                let extra_record = Type::Record(
                    remaining1
                        .into_iter()
                        .map(|(k, v)| ((*k).clone(), (*v).clone()))
                        .collect(),
                    None,
                );
                let s = Subst::singleton(rv2.clone(), extra_record);
                Ok(s.compose(subst))
            }
            (true, false, Some(rv1), None) => {
                let extra_record = Type::Record(
                    remaining2
                        .into_iter()
                        .map(|(k, v)| ((*k).clone(), (*v).clone()))
                        .collect(),
                    None,
                );
                let s = Subst::singleton(rv1.clone(), extra_record);
                Ok(s.compose(subst))
            }
            (_, _, Some(rv1), Some(rv2)) => {
                let s = Self::unify_var(rv1, &Type::Var(rv2.clone()), registry, class_registry)?;
                Ok(s.compose(subst))
            }
            _ => Err(Self::unification_error(
                format!(
                    "record with fields: {}",
                    fields1.iter().map(|(k, _)| k).cloned().collect::<Vec<_>>().join(", ")
                ),
                format!(
                    "record with fields: {}",
                    fields2.iter().map(|(k, _)| k).cloned().collect::<Vec<_>>().join(", ")
                ),
            )),
        }
    }
}
