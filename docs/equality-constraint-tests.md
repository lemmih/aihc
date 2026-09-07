# Equality constraint tests

The `equality-constraint-*.yaml` fixtures specify the required behavior of `~`.
GHC 9.12.4 provides the reference results.
GHC accepts all 26 positive programs and evaluates each `result` to `On`.
GHC rejects all six negative programs.

## Test stages

| Stage | Fixture directory | Required result |
| --- | --- | --- |
| Resolve | `components/aihc-resolve/test/Test/Fixtures/golden` | Each equality use has the type namespace identity `GHC.Types`. |
| TC | `components/aihc-tc/test/Test/Fixtures/annotated` | Each program has the same type-check result as GHC. |
| FC | `bin/aihc/compiler/fc/test/Test/Fixtures/golden` | Desugaring, FC lint, and the FC text round trip succeed. |
| GRIN evaluation | `test/Test/Fixtures/eval` | The lowered program evaluates to `On`. |

The resolver fixtures supply a minimal `GHC.Types` class declaration.
This declaration represents the exported class for name resolution only.
The GHC reference check uses the installed `GHC.Types` module instead.
The TC, FC, and GRIN fixtures use the project libraries without a substitute equality class.

All ten resolver fixtures pass.
Equality syntax uses the exported `GHC.Types` identity without an explicit import.
Nine of the original 32 TC fixtures pass.
The other 23 TC fixtures still have XFAIL status.
All 26 FC fixtures and all 26 GRIN fixtures still have XFAIL status.
These failures now expose separate solver and evidence defects.

One additional TC fixture verifies that a user class named `~` remains an ordinary class.
GHC 9.12.4 accepts this fixture with `NoImplicitPrelude`.

## Cases

The positive cases cover these properties:

- Reflexivity, symmetry, transitivity, and casts from given equality constraints.
- Equality through lists and functions, list decomposition, and equality between type constructors.
- Prefix syntax, qualified syntax, explicit type imports, and constraint synonyms.
- Equality superclasses and equality constraints in instance contexts.
- GADT constraints and existential fields.
- Higher-rank arguments, local signatures, and visible type arguments.
- Open type families, closed type families, and given type family equalities.
- Promoted constructors and polymorphic kinds.
- Equality evidence across module boundaries.
- An unused divergent argument that must remain unevaluated.

The negative cases cover distinct data types, newtype nominal equality, absent evidence, the occurs check, incompatible kinds, and non-injective type families.
The resolver cases also cover `NoImplicitPrelude`, hidden imports, GADT contexts, and superclass contexts.

## Fixture assertions

If a TC fixture omits `annotated`, it requires successful parsing, name resolution, and type checks.
If it supplies `annotated`, it compares the exact annotation snapshot, including type diagnostics.
Negative TC fixtures use diagnostic snapshots to specify the required type errors.
A parse error or resolve error cannot satisfy a diagnostic snapshot.

If an FC fixture omits `expected`, it checks successful compilation without an output snapshot.
This assertion still requires all normal compiler checks, FC lint, and the FC text round trip.
If an FC fixture supplies `expected`, it compares exact FC text, including an explicitly empty snapshot.

The three `fixture-*` control fixtures check successful TC, a TC diagnostic snapshot, and successful FC assertions.
These fixtures use ordinary source programs.

For either assertion, future success changes an XFAIL result to XPASS and fails the test suite.
The positive equality fixtures require no guessed output snapshots.

## Reference checks

For a GHC check, extract each fixture module into its corresponding `.hs` file.
Pass each fixture extension to GHC with `-X`.
Add `-package ghc-prim` for direct `GHC.Types` imports.
Use `-fno-code` to check types.
For a positive program, evaluate `result` and check that it returns `On`.
For a negative program, check that GHC reports the expected type error.

For resolver fixtures, omit the substitute `GHC.Types` source.
Rename the test module `Main` to `Test` to avoid a required `main` definition.
Compile the remaining modules against GHC's installed `GHC.Types`.

Run the project checks with `just check`.
