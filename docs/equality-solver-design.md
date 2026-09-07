# Equality solver foundation

The equality solver controls nominal Haskell equality in `aihc-tc`.
FC converts checked evidence and validates System FC rules.
GRIN preserves the semantics of that FC program.

## Structural decomposition

`decomposeNominalEquality` defines the common structural rules.
The direct unifier, wanted solver, and given solver use these rules.
Constructor pattern instantiation also uses these rules to match result indices.

A nominal equality between applications of the same data type constructor permits equality of their type arguments.
Function types permit equality of their domains and ranges.
Type applications permit equality of their functions and arguments when no saturated family application obstructs decomposition.

A type family application does not permit equality of its family arguments.
The solver separates extra arguments from the saturated family application before decomposition.
The same restriction applies at each depth of a type.

The caller retains responsibility for the proof of each constraint.
A list of child equalities alone is not a proof of the parent equality.

## Constructor indices

Constructor pattern instantiation first matches the constructor result against the scrutinee type.
The match uses only nominal decomposition rules.
It reuses each determined type argument from the scrutinee.
It does not replace a determined rigid index with an assignable metavariable.

For a repeated constructor index, the match retains the first type argument.
The result constraint retains the equality with each later occurrence.
A family application cannot determine a constructor type argument through decomposition.

## Evidence contract

The following requirements define the next stage.

Each solved wanted equality must have a coercion between its original endpoints under its evidence scope.
Each child constraint must have a separate evidence variable.
The parent coercion must use the child coercions.
Reflexivity is valid only when both endpoints are the same after metavariable substitution.

Each type family reduction must return its axiom evidence with its result.
Each rewrite through a given equality must retain the evidence for that rewrite.
Superclass equality must retain its dictionary projection.

Each coercion constructor must specify its endpoint, kind, and role rules.
FC must validate those rules independently.
Nominal coercions must not use representation equality as nominal evidence.
Projection must not infer family injectivity from the shape of an FC type application.
FC will need explicit constructor facts for this check.

## Scope contract

Each implication must retain its skolems, given evidence binders, wanted constraints, and creation level.
Each metavariable must retain its creation level and kind.
The solver must check assignment permissions before a metavariable assignment.
Local assumptions must not permit an invalid assignment to an outer metavariable.
Local skolems must not escape their implication.

Residual constraints can move to an outer scope only when their types and evidence do not depend on the local scope.
Nested implications and local generalisation must use the same scope rules.

## Test gates

Use source fixtures for each rule and its limit conditions.
Compare valid and invalid source programs with the reference GHC version.
Include reversed constraint order, nested types, repeated indices, and nested implications.

For positive programs, check TC acceptance, FC lint, the FC text round trip, and evaluation separately.
For negative programs, check the required TC diagnostic.
An evaluation result alone does not validate equality evidence.

Run `just fmt` before `just check`.
Commit only after `just check` succeeds.
