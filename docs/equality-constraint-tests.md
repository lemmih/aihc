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

Five resolver fixtures pass through explicit imports, qualified names, or a re-export.
Five resolver fixtures have XFAIL status because equality syntax needs an explicit import in AIHC.
All equality TC, FC, and GRIN fixtures currently have XFAIL status because AIHC cannot resolve `~`.
Later corrections can expose separate errors in each downstream stage.

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

A TC fixture can specify `typecheck: true` or `typecheck: false` instead of an annotation snapshot.
This assertion requires successful parsing and name resolution before it examines the type diagnostics.
Thus, a resolve error cannot satisfy `typecheck: false`.
Existing fixtures still use exact annotation snapshots when they omit `typecheck`.

If an FC fixture omits `expected`, it checks successful compilation without an output snapshot.
This assertion still requires all normal compiler checks, FC lint, and the FC text round trip.
If an FC fixture supplies `expected`, it compares exact FC text, including an explicitly empty snapshot.

The three `fixture-*` control fixtures check successful TC, rejected TC, and successful FC assertions.
These fixtures use ordinary source programs.

For either assertion, future success changes an XFAIL result to XPASS and fails the test suite.
No guessed output snapshot can keep a corrected equality case at XFAIL.

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
