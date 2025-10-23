# Hindley–Milner Type Systems

Hindley–Milner (HM) is the classical polymorphic type system that powers languages such as ML, OCaml, and early versions of Haskell. It strikes a balance between expressiveness (parametric polymorphism) and tractable, annotation-free type inference.

## Overview

**Parametric polymorphism**: functions can operate uniformly over many types without runtime overhead[^1].

**Type inference**: the compiler deduces the most general (principal) type scheme for each expression[^1].

**Declarative typing judgment**: The typing judgment \\(\Gamma \vdash e : \sigma\\) relates a context \\( \Gamma \\), an expression \\( e \\), and a type scheme \\( \sigma \\).

The result is a system where generic programs remain statically typed without drowning the developer in annotations.

## Core Concepts

### Why HM?

\\(\lambda\\)-calculus requires explicit annotations to achieve polymorphism. HM extends the calculus with *let-polymorphism* and carefully restricted generalization so that inference stays decidable and efficient.

### Monotypes vs Polytypes

**Monotypes** (\\(\tau\\)): concrete types such as \\(\alpha\\), \\(\text{Int} \to \text{Bool}\\), or constructor applications \\(C\,\tau_1\cdots\tau_n\\)[^3].

**Polytypes / type schemes** (\\(\sigma\\)): quantifications over monotypes, e.g. \\(\forall \alpha.\,\alpha \to \alpha\\).

**Principal type**: every well-typed expression has a unique (up to renaming) most general type scheme from which all other valid typings can be instantiated[^1].

### Generalization and Instantiation

**Generalization**: close a monotype over the free type variables not present in the environment.

**Instantiation**: specialise a polytype by substituting quantified variables with fresh monotype variables.

### Let-Polymorphism

Only `let`-bound definitions are generalized. Lambda parameters remain monomorphic in HM; this restriction is critical to keep inference decidable[^1].

## Formal Skeleton

### Syntax

```text
e ::= x
    | λ x. e
    | e₁ e₂
    | let x = e₁ in e₂
```

The associated type grammar and typing environments are:

\\[
\begin{aligned}
\tau &::= \alpha \mid C(\tau_1,\dots,\tau_n) \mid \tau \to \tau \\
\sigma &::= \tau \mid \forall \alpha.\,\sigma \\
\Gamma &::= \emptyset \mid \Gamma, x : \sigma
\end{aligned}
\\]

### Typing Rules

Typing judgments take the form \\(\Gamma \vdash e : \sigma\\). Core rules include:

\\[
\frac{x : \sigma \in \Gamma}{\Gamma \vdash x : \sigma}
\quad\text{(Var)}
\\]

\\[
\frac{\Gamma, x : \tau \vdash e : \tau'}{\Gamma \vdash \lambda x.\,e : \tau \to \tau'}
\quad\text{(Abs)}
\\]

\\[
\frac{\Gamma \vdash e_0 : \tau \to \tau' \qquad \Gamma \vdash e_1 : \tau}{\Gamma \vdash e_0\,e_1 : \tau'}
\quad\text{(App)}
\\]

\\[
\frac{\Gamma \vdash e_0 : \sigma \qquad \Gamma, x : \sigma \vdash e_1 : \tau}{\Gamma \vdash \text{let } x = e_0 \text{ in } e_1 : \tau}
\quad\text{(Let)}
\\]

\\[
\frac{\Gamma \vdash e : \sigma' \qquad \sigma' \sqsubseteq \sigma}{\Gamma \vdash e : \sigma}
\quad\text{(Inst)}
\\]

\\[
\frac{\Gamma \vdash e : \sigma \qquad \alpha \notin \mathrm{free}(\Gamma)}{\Gamma \vdash e : \forall \alpha.\,\sigma}
\quad\text{(Gen)}
\\]

Here \\(\sigma' \sqsubseteq \sigma\\) means that \\(\sigma'\\) is an instance of \\(\sigma\\) (obtained by instantiating quantified variables)[^1].

## Algorithm W (Inference Sketch)

Algorithm W is the archetypal inference engine for HM[^2].

1. **Annotate sub-expressions** with fresh type variables.
2. **Collect constraints** when traversing the AST (especially from applications).
3. **Unify** constraints to solve for unknown types.
4. **Generalize** at each `let` by quantifying over variables not free in the environment.
5. **Return** the principal type scheme produced by the substitutions.

Typical programs are handled in near-linear time, although the theoretical worst case is higher[^1].

## Strengths and Limitations

### Strengths

Minimal annotations with strong static guarantees.

Principled parametric polymorphism with predictable runtime behaviour.

A deterministic, well-understood inference algorithm.

### Limitations

No native subtyping; adding it naively renders inference undecidable[^1].

Higher-rank polymorphism (e.g., passing polymorphic functions as arguments) requires extensions that typically sacrifice automatic inference.

Recursive bindings and mutation demand additional care to avoid unsound generalization.

## Extensions: Type Classes

Many ML-derived languages extend HM with **type classes** to model constrained polymorphism[^5]. Type classes capture ad-hoc behavior (equality, ordering, pretty-printing) without abandoning the core inference model.

### Motivation

Developers often need functions that work only for types supporting specific operations (equality, ordering, etc.).

Type classes describe those obligations once and then allow generic code to depend on them declaratively.

### Integration with HM

A type class \\(C\\) packages a set of operations. A type \\(T\\) becomes an instance of \\(C\\) by providing implementations.

Type schemes gain constraint contexts, e.g. \\(\forall a.\,(Eq\,a) \Rightarrow a \to a\\), read as “for all \\(a\\) that implement `Eq`, this function maps \\(a\\) to \\(a\\)”.

Environments track both type bindings and accumulated constraints, written informally as \\(\Gamma \vdash e : \sigma \mid \Delta\\).

During generalization, constraints that do not mention the generalized variables can be abstracted over; during instantiation, remaining constraints must be satisfied (dictionary passing, instance resolution, etc.).

Type classes preserve type safety while keeping user code concise, but introduce design questions about coherence (no conflicting instances), instance search termination, and tooling ergonomics.

## Extensions: Higher-Rank Types

Higher-rank polymorphism allows universal quantifiers to appear inside function arguments, enabling functions that consume polymorphic functions[^6].

HM is rank-1: all \\(\forall\\) quantifiers appear at the outermost level.

### Why Higher Rank?

Certain abstractions require accepting *polymorphic* functions as arguments, e.g.

  ```haskell
  applyTwice :: (forall a. a -> a) -> Int -> Int
  applyTwice f x = f (f x)
  ```

HM cannot express this because the quantifier lives to the left of an arrow. Extending to rank-2 (or higher) types unlocks APIs like `runST :: ∀a.(∀s. ST s a) -> a`[^8].

### Typing Considerations

The grammar generalizes to allow quantified types within arrow positions; checking such programs typically relies on **bidirectional type checking**[^9].

Full type inference for arbitrary rank is undecidable; practical compilers require annotations or rely on heuristics[^7].

Despite the cost, higher-rank types enable powerful encapsulation patterns and stronger invariants.

### Design Trade-offs

**Pros**: Expressiveness for APIs manipulating polymorphic functions; better information hiding (e.g., `ST`).

**Cons**: Additional annotations, more complex error messages, heavier implementation burden.

## Further Reading

Implementing HM [Stimsina](https://blog.stimsina.com/posts/hindley-milner/)

Parametricity and type classes [Well-Typed](https://well-typed.com/blog/2015/08/parametricity-part2/)

[^1]: [Wikipedia: HM Type System](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)
[^2]: [MIT OpenCourseWare](https://ocw.mit.edu/courses/6-821-programming-languages-fall-2002/resources/lecture-5-hindley-milner-type-system/)
[^3]: [Columbia Computer Science](https://www.cs.columbia.edu/~sedwards/classes/2019/4115-fall/pldi76-milner.pdf)
[^5]: [Wikipedia: Type class](https://en.wikipedia.org/wiki/Type_class)
[^6]: [GHC User Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rank_polymorphism.html)
[^7]: [Practical type inference for arbitrary-rank types Peyton Jones et al.](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf)
[^8]: [Higher Rank Polymorphism](https://8thlight.com/insights/higher-rank-polymorphism)
[^9]: [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism Dunfield, and Krishnaswami.](https://arxiv.org/abs/1306.6032)
