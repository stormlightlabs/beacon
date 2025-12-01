use beacon_core::{Subst, Type, TypeCtor, TypeVar, TypeVarConstraintRegistry, unify::Unifier};
use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use std::hint::black_box;

/// Benchmark unification of simple types
fn bench_simple_unification(c: &mut Criterion) {
    let registry = TypeVarConstraintRegistry::new();

    c.bench_function("unify_int_int", |b| {
        b.iter(|| {
            let t1 = Type::Con(TypeCtor::Int);
            let t2 = Type::Con(TypeCtor::Int);
            Unifier::unify(black_box(&t1), black_box(&t2), black_box(&registry))
        })
    });

    c.bench_function("unify_typevar_int", |b| {
        b.iter(|| {
            let t1 = Type::Var(TypeVar::new(0));
            let t2 = Type::Con(TypeCtor::Int);
            Unifier::unify(black_box(&t1), black_box(&t2), black_box(&registry))
        })
    });
}

/// Benchmark unification of generic types with varying complexity
fn bench_generic_type_unification(c: &mut Criterion) {
    let registry = TypeVarConstraintRegistry::new();

    let mut group = c.benchmark_group("generic_unification");

    group.bench_function("list_typevar", |b| {
        b.iter(|| {
            let t1 = Type::App(
                Box::new(Type::Con(TypeCtor::List)),
                Box::new(Type::Var(TypeVar::new(0))),
            );
            let t2 = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Int)));
            Unifier::unify(black_box(&t1), black_box(&t2), black_box(&registry))
        })
    });

    group.bench_function("dict_two_typevars", |b| {
        b.iter(|| {
            let t1 = Type::App(
                Box::new(Type::App(
                    Box::new(Type::Con(TypeCtor::Dict)),
                    Box::new(Type::Var(TypeVar::new(0))),
                )),
                Box::new(Type::Var(TypeVar::new(1))),
            );
            let t2 = Type::App(
                Box::new(Type::App(
                    Box::new(Type::Con(TypeCtor::Dict)),
                    Box::new(Type::Con(TypeCtor::String)),
                )),
                Box::new(Type::Con(TypeCtor::Int)),
            );
            Unifier::unify(black_box(&t1), black_box(&t2), black_box(&registry))
        })
    });

    group.finish();
}

/// Benchmark function type unification
fn bench_function_type_unification(c: &mut Criterion) {
    let registry = TypeVarConstraintRegistry::new();

    let mut group = c.benchmark_group("function_unification");

    group.bench_function("simple_function", |b| {
        b.iter(|| {
            let t1 = Type::Fun(
                vec![("x".to_string(), Type::Con(TypeCtor::Int))],
                Box::new(Type::Con(TypeCtor::String)),
            );
            let t2 = Type::Fun(
                vec![("x".to_string(), Type::Con(TypeCtor::Int))],
                Box::new(Type::Con(TypeCtor::String)),
            );
            Unifier::unify(black_box(&t1), black_box(&t2), black_box(&registry))
        })
    });

    group.bench_function("multi_arg_function", |b| {
        b.iter(|| {
            let t1 = Type::Fun(
                vec![
                    ("x".to_string(), Type::Con(TypeCtor::Int)),
                    ("y".to_string(), Type::Con(TypeCtor::String)),
                    ("z".to_string(), Type::Con(TypeCtor::Bool)),
                ],
                Box::new(Type::App(
                    Box::new(Type::Con(TypeCtor::List)),
                    Box::new(Type::Con(TypeCtor::Int)),
                )),
            );
            let t2 = t1.clone();
            Unifier::unify(black_box(&t1), black_box(&t2), black_box(&registry))
        })
    });

    group.bench_function("generic_function", |b| {
        b.iter(|| {
            let t1 = Type::Fun(
                vec![("x".to_string(), Type::Var(TypeVar::new(0)))],
                Box::new(Type::App(
                    Box::new(Type::Con(TypeCtor::List)),
                    Box::new(Type::Var(TypeVar::new(0))),
                )),
            );
            let t2 = Type::Fun(
                vec![("x".to_string(), Type::Con(TypeCtor::Int))],
                Box::new(Type::App(
                    Box::new(Type::Con(TypeCtor::List)),
                    Box::new(Type::Con(TypeCtor::Int)),
                )),
            );
            Unifier::unify(black_box(&t1), black_box(&t2), black_box(&registry))
        })
    });

    group.finish();
}

/// Benchmark nested type unification
fn bench_nested_type_unification(c: &mut Criterion) {
    let registry = TypeVarConstraintRegistry::new();

    let mut group = c.benchmark_group("nested_unification");

    for depth in [1, 3, 5, 7].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(depth), depth, |b, &depth| {
            b.iter(|| {
                let mut t1 = Type::Con(TypeCtor::Int);
                let mut t2 = Type::Con(TypeCtor::Int);

                for _ in 0..depth {
                    t1 = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(t1));
                    t2 = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(t2));
                }

                Unifier::unify(black_box(&t1), black_box(&t2), black_box(&registry))
            })
        });
    }

    group.finish();
}

/// Benchmark substitution composition
fn bench_substitution_composition(c: &mut Criterion) {
    let mut group = c.benchmark_group("substitution");

    for size in [5, 10, 20, 50].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.iter(|| {
                let mut subst = Subst::empty();

                for i in 0..size {
                    let var = TypeVar::new(i as u32);
                    let ty = Type::Con(TypeCtor::Int);
                    let new_subst = Subst::singleton(var, ty);
                    subst = new_subst.compose(subst);
                }

                black_box(subst)
            })
        });
    }

    group.finish();
}

/// Benchmark type application
fn bench_type_application(c: &mut Criterion) {
    let mut group = c.benchmark_group("type_application");

    for size in [5, 10, 20, 50].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.iter(|| {
                let mut subst = Subst::empty();
                for i in 0..size {
                    let new_subst = Subst::singleton(TypeVar::new(i as u32), Type::Con(TypeCtor::Int));
                    subst = new_subst.compose(subst);
                }

                let ty = Type::App(
                    Box::new(Type::Con(TypeCtor::List)),
                    Box::new(Type::Var(TypeVar::new(0))),
                );

                black_box(subst.apply(&ty))
            })
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_simple_unification,
    bench_generic_type_unification,
    bench_function_type_unification,
    bench_nested_type_unification,
    bench_substitution_composition,
    bench_type_application
);

criterion_main!(benches);
