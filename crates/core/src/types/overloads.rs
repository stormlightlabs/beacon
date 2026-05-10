//! Function and method overload sets.

use super::Type;

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

    /// Resolve the best matching overload for the given argument types. This implements Python's overload resolution semantics.
    ///
    /// Returns the first signature where all argument types are subtypes of the parameters.
    ///
    /// For method overloads, both signatures and arg_types should exclude the self parameter.
    /// Signatures are typically extracted without self (as self often lacks type annotations).
    pub fn resolve(&self, arg_types: &[Type]) -> Option<&Type> {
        for signature in &self.signatures {
            if let Type::Fun(params, _) = signature {
                if params.len() != arg_types.len() {
                    continue;
                }

                let all_match = arg_types
                    .iter()
                    .zip(params.iter())
                    .all(|(arg, (_, param))| arg.is_subtype_of(param));

                if all_match {
                    return Some(signature);
                }
            }
        }

        self.implementation.as_ref()
    }
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
