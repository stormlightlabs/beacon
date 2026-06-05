//! Function and method overload sets.

use super::{FunctionParam, FunctionParamKind, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OverloadSet {
    /// The overload signatures (from @overload decorators)
    /// These are tried in order during overload resolution
    pub signatures: Vec<Type>,

    /// The implementation signature (optional)
    /// This is the actual function that will be called
    pub implementation: Option<Type>,
}

impl OverloadSet {
    /// Create a new overload set with the given signatures
    pub fn new(signatures: Vec<Type>) -> Self {
        Self { signatures, implementation: None }
    }

    /// Create an overload set with signatures and an implementation
    pub fn with_implementation(signatures: Vec<Type>, implementation: Type) -> Self {
        Self { signatures, implementation: Some(implementation) }
    }

    /// Add an overload signature to this set
    pub fn add_signature(&mut self, signature: Type) {
        self.signatures.push(signature);
    }

    /// Set the implementation signature
    pub fn set_implementation(&mut self, implementation: Type) {
        self.implementation = Some(implementation);
    }

    /// Resolve the best matching overload for positional argument types.
    ///
    /// Returns the first signature where all provided argument types are
    /// compatible with the corresponding parameters. If no overload matches,
    /// returns the implementation signature as a conservative fallback.
    pub fn resolve(&self, arg_types: &[Type]) -> Option<&Type> {
        for signature in &self.signatures {
            if signature_accepts_args(signature, arg_types) {
                return Some(signature);
            }
        }

        self.implementation.as_ref()
    }

    /// Return pairs of overload indices whose accepted argument domains overlap.
    pub fn overlapping_pairs(&self) -> Vec<(usize, usize)> {
        let mut overlaps = Vec::new();
        for i in 0..self.signatures.len() {
            for j in (i + 1)..self.signatures.len() {
                if signatures_overlap(&self.signatures[i], &self.signatures[j]) {
                    overlaps.push((i, j));
                }
            }
        }
        overlaps
    }

    /// Validate that the concrete implementation can accept every overload and
    /// can return a value compatible with each overload's advertised return.
    pub fn implementation_compatibility_errors(&self) -> Vec<String> {
        let Some(implementation) = &self.implementation else {
            return Vec::new();
        };

        self.signatures
            .iter()
            .enumerate()
            .filter_map(|(idx, signature)| {
                implementation_compatibility_error(signature, implementation)
                    .map(|err| format!("overload {idx}: {err}"))
            })
            .collect()
    }
}

fn signature_accepts_args(signature: &Type, arg_types: &[Type]) -> bool {
    let Some((params, _)) = function_parts(signature) else {
        return false;
    };

    let positional_params: Vec<_> = params
        .iter()
        .filter(|param| {
            matches!(
                param.kind,
                FunctionParamKind::PositionalOnly | FunctionParamKind::PositionalOrKeyword
            )
        })
        .collect();
    let required_positional = positional_params.iter().filter(|param| !param.has_default).count();
    let has_varargs = params
        .iter()
        .any(|param| matches!(param.kind, FunctionParamKind::VarArgs));

    if arg_types.len() < required_positional {
        return false;
    }
    if !has_varargs && arg_types.len() > positional_params.len() {
        return false;
    }

    arg_types.iter().enumerate().all(|(idx, arg)| {
        let param_ty = positional_params.get(idx).map(|param| &param.ty).or_else(|| {
            params
                .iter()
                .find(|param| matches!(param.kind, FunctionParamKind::VarArgs))
                .map(|param| &param.ty)
        });
        param_ty.is_some_and(|param_ty| arg.is_subtype_of(param_ty))
    })
}

fn signatures_overlap(left: &Type, right: &Type) -> bool {
    let Some((left_params, _)) = function_parts(left) else {
        return false;
    };
    let Some((right_params, _)) = function_parts(right) else {
        return false;
    };

    let left_pos = positional_params(&left_params);
    let right_pos = positional_params(&right_params);
    if required_count(&left_pos) > right_pos.len() || required_count(&right_pos) > left_pos.len() {
        return false;
    }

    left_pos
        .iter()
        .zip(right_pos.iter())
        .all(|(left, right)| left.ty.is_subtype_of(&right.ty) || right.ty.is_subtype_of(&left.ty))
}

fn implementation_compatibility_error(signature: &Type, implementation: &Type) -> Option<String> {
    let Some((sig_params, sig_ret)) = function_parts(signature) else {
        return Some("signature is not a function".to_string());
    };
    let Some((impl_params, impl_ret)) = function_parts(implementation) else {
        return Some("implementation is not a function".to_string());
    };

    let sig_pos = positional_params(&sig_params);
    let impl_pos = positional_params(&impl_params);
    if required_count(&impl_pos) > required_count(&sig_pos) || impl_pos.len() < sig_pos.len() {
        return Some("implementation parameters are not broad enough".to_string());
    }

    for (sig_param, impl_param) in sig_pos.iter().zip(impl_pos.iter()) {
        if !sig_param.ty.is_subtype_of(&impl_param.ty) {
            return Some(format!(
                "implementation parameter '{}' of type {} cannot accept overload parameter '{}' of type {}",
                impl_param.name, impl_param.ty, sig_param.name, sig_param.ty
            ));
        }
    }

    if !sig_ret.is_subtype_of(impl_ret) {
        return Some(format!(
            "implementation return type {impl_ret} is not broad enough for overload return {sig_ret}"
        ));
    }

    None
}

fn function_parts(ty: &Type) -> Option<(Vec<FunctionParam>, &Type)> {
    match ty {
        Type::Fun(params, ret) => Some((
            params
                .iter()
                .map(|(name, ty)| {
                    FunctionParam::with_metadata(
                        name.clone(),
                        ty.clone(),
                        FunctionParamKind::PositionalOrKeyword,
                        false,
                    )
                })
                .collect(),
            ret,
        )),
        Type::FunWithParams(params, ret) => Some((params.clone(), ret)),
        _ => None,
    }
}

fn positional_params(params: &[FunctionParam]) -> Vec<&FunctionParam> {
    params
        .iter()
        .filter(|param| {
            matches!(
                param.kind,
                FunctionParamKind::PositionalOnly | FunctionParamKind::PositionalOrKeyword
            )
        })
        .collect()
}

fn required_count(params: &[&FunctionParam]) -> usize {
    params.iter().filter(|param| !param.has_default).count()
}

impl std::fmt::Display for OverloadSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Overload[")?;
        for (i, sig) in self.signatures.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{sig}")?;
        }
        write!(f, "]")
    }
}
