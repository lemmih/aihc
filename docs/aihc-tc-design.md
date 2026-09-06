# aihc-tc: Type Checker Design

High-level design for the aihc type checker component, based on the
OutsideIn(X) algorithm.

---

## 1. Goals and non-goals

### Goals

- Implement OutsideIn(X)-style type inference with support for
  polymorphism, type classes, GADTs, and type families.
- Annotate the existing surface AST (`Aihc.Parser.Syntax`) with typing
  information: inferred types, type applications, type abstractions,
  coercions, and evidence bindings.
- Produce enough information for a downstream desugaring pass to construct
  an explicit System FC core, without performing that desugaring itself.
- Provide clear, provenance-tracked error messages.

### Non-goals

- No desugaring to core IR (that is `aihc-desugar`).
- No code generation.
- No runtime representation decisions.
- No deriving-specific lowering. The TC selects the deriving strategy,
  infers the instance context, and then writes each derived instance as an
  ordinary `instance` declaration (see `Aihc.Tc.Deriving.Generate`), which
  the instance checker and later phases treat like source.

---

## 2. Place in the pipeline

```
source
  -> aihc-cpp      (preprocessing)
  -> aihc-parser   (lexing + parsing -> surface AST)
  -> aihc-resolve  (name resolution -> annotated AST)
  -> aihc-tc       (type checking -> annotated AST with types/evidence)
  -> aihc-desugar  (future: explicit System FC core)
```

The type checker consumes a name-resolved AST and produces the same AST
annotated with typing information. It does not transform the tree structure.

---

## 3. Output model: annotations, not a new AST

Following the pattern established by `aihc-resolve`, the type checker
attaches its results as `Annotation` values on AST nodes using the existing
`DeclAnn`/`EAnn`/`PAnn`/`TAnn` wrappers and `Dynamic`-typed `Annotation`.

This means:

- The surface AST type (`Module`, `Decl`, `Expr`, etc.) is unchanged.
- Typing information is retrievable via pattern synonyms (like
  `EResolution` in `aihc-resolve`).
- Downstream passes can extract typing information without depending on a
  separate typed AST type.

### 3.1 What the annotations carry

Each annotated node carries a `TcAnnotation` record that includes whichever
of the following are applicable:

```haskell
data TcAnnotation = TcAnnotation
  { tcAnnType       :: !Type           -- inferred/checked type of this node
  , tcAnnEvidence   :: ![EvBinding]    -- evidence bindings scoped here
  , tcAnnCoercions  :: ![CoercionInfo] -- coercions needed at this node
  , tcAnnTyApps     :: ![TcType]       -- explicit type applications
  , tcAnnTyAbs      :: ![TyVar]        -- type variables abstracted here
  , tcAnnDictAbs    :: ![EvVar]        -- dictionary parameters abstracted here
  , tcAnnOrigin     :: !CtOrigin       -- provenance for error reporting
  }
```

Not every field is populated for every node. A variable reference gets a
type and possibly type applications; a top-level binding gets type/dict
abstractions; a case branch gets coercion evidence from GADT refinement.

### 3.2 What downstream desugaring needs

The annotations must carry enough information for `aihc-desugar` to
produce an explicitly typed core term. Specifically:

| Surface construct       | TC annotation provides                              |
|-------------------------|-----------------------------------------------------|
| Polymorphic binding     | Quantified type vars, dictionary params to abstract  |
| Variable reference      | Instantiation types, evidence arguments              |
| Function application    | Result type                                          |
| Lambda / match          | Argument types, result type                          |
| Let binding             | Monomorphic type or generalized scheme               |
| Case scrutinee          | Scrutinee type                                       |
| Case branch (GADT)      | Skolem vars, given equalities, given dictionaries    |
| Type family application | Coercion evidence for reduction                      |
| Type annotation         | Checked type, coercion if subsumption needed         |

---

## 4. Internal type representation

The TC needs its own type representation that is richer than the surface
`Aihc.Parser.Syntax.Type`. Surface types are syntax; internal types are
semantic.

```haskell
-- Semantic types used during type checking.
-- The 's' parameter threads from STRef through every type that
-- transitively contains a meta-variable.  It is confined to the
-- internals of the TC; output types are 's'-free after zonking.
data TcType s
  = TcTyVar   (TyVar s)                  -- rigid (skolem) or flexible (meta)
  | TcTyCon   TyCon [TcType s]           -- saturated or partially applied
  | TcFunTy   (TcType s) (TcType s)      -- function type
  | TcForAllTy (TyVar s) (TcType s)      -- universal quantification
  | TcQualTy  [Pred s] (TcType s)        -- qualified type (constraints =>)
  | TcLitTy   TyLit                      -- type-level literals
  | TcAppTy   (TcType s) (TcType s)      -- unsaturated type application

data TyVar s = TyVar
  { tvName    :: !Name
  , tvUnique  :: !Unique
  , tvKind    :: !(TcType s)
  , tvFlavor  :: !(TyVarFlavor s)
  }

data TyVarFlavor s
  = SkolemTv          -- rigid, from quantification or GADT match
  | MetaTv !(MetaRef s)  -- unification variable (mutable cell)
  | RuntimeTv         -- bound at runtime (lambda-bound type var)

type MetaRef s = STRef s (Maybe (TcType s))

data TypeScheme s = ForAll [TyVar s] [Pred s] (TcType s)
```

### 4.1 Conversion from surface types

A function `surfaceToTcType :: Aihc.Parser.Syntax.Type -> TcM s (TcType s)`
converts parsed type syntax into the internal representation during kind
checking. This runs before or as part of constraint generation.

---

## 5. Predicates, evidence, and coercions

### 5.1 Predicates

```haskell
data Pred s
  = ClassPred ClassName [TcType s]    -- e.g. Eq a
  | EqPred    (TcType s) (TcType s)   -- e.g. a ~ Bool
  | IParamPred Text (TcType s)        -- implicit parameters, e.g. ?x :: Int
```

### 5.2 Evidence

```haskell
data EvTerm s
  = EvVar      EvVar                          -- reference to evidence variable
  | EvDict     ClassName [TcType s] [EvTerm s]  -- dictionary construction
  | EvSuperClass (EvTerm s) Int               -- superclass selection
  | EvCoercion (Coercion s)                   -- coercion as evidence
  | EvCast     (EvTerm s) (Coercion s)        -- cast evidence
  | EvLit      EvLiteral                      -- known-at-compile-time evidence

data EvBinding s = EvBinding
  { evBindVar  :: !EvVar
  , evBindTerm :: !(EvTerm s)
  }
```

### 5.3 Coercions

```haskell
data Coercion s
  = CoVar       CoVar                           -- coercion variable
  | Refl        (TcType s)                      -- reflexivity
  | Sym         (Coercion s)                    -- symmetry
  | Trans       (Coercion s) (Coercion s)       -- transitivity
  | TyConAppCo  TyCon [Coercion s]              -- lift through type constructor
  | AppCo       (Coercion s) (Coercion s)       -- application
  | ForAllCo    (TyVar s) (Coercion s) (Coercion s)  -- under forall
  | AxiomInstCo AxiomName [TcType s]            -- type family / newtype axiom
  | NthCo       Int (Coercion s)                -- projection
  | SubCo       (Coercion s)                    -- nominal -> representational
```

---

## 6. Constraints

```haskell
data Ct s = Ct
  { ctPred     :: !(Pred s)
  , ctFlavor   :: !CtFlavor
  , ctEvidence :: !(CtEvidence s)
  , ctOrigin   :: !CtOrigin
  , ctLoc      :: !SourceSpan
  }

data CtFlavor
  = Given       -- from GADT match or user annotation
  | Wanted      -- must be solved
  | Derived     -- inferred, no evidence needed

data CtEvidence s = CtEvidence
  { ctevPred :: !(Pred s)
  , ctevDest :: !EvDest
  }

data EvDest
  = EvBind EvVar        -- fill this variable with evidence
  | EvHole EvVar        -- evidence hole (to be filled later)

data Implication s = Implication
  { implSkols     :: ![TyVar s]       -- skolem type variables
  , implGivenEvs  :: ![EvVar]         -- evidence variables for givens
  , implGivenCts  :: ![Ct s]          -- given constraints
  , implWantedCts :: ![Ct s]          -- wanted constraints
  , implTcLevel   :: !TcLevel         -- nesting level
  , implInfo      :: !ImplOrigin      -- what caused this implication
  }
```

### 6.1 Constraint origins

```haskell
data CtOrigin
  = OccurrenceOf Name               -- using a name
  | AppOrigin SourceSpan             -- function application
  | LambdaOrigin SourceSpan          -- lambda binding
  | PatternOrigin SourceSpan         -- pattern match
  | CaseBranchOrigin SourceSpan      -- GADT branch
  | SigOrigin SourceSpan             -- type signature check
  | InstOrigin ClassName             -- instance resolution
  | DerivOrigin                      -- deriving
  | DefaultOrigin                    -- defaulting
```

These are carried on every constraint and used for error reporting.

---

## 7. Environments

### 7.1 Global environment

Built once per module (or set of modules) from the parsed declarations:

```haskell
data GlobalEnv s = GlobalEnv
  { geTyCons       :: !(Map TyConName TyConInfo)
  , geDataCons     :: !(Map DataConName (DataConInfo s))
  , geClasses      :: !(Map ClassName (ClassInfo s))
  , geInstances    :: ![InstanceInfo s]
  , geFamAxioms    :: ![FamAxiom s]
  , geTypeSynonyms :: !(Map Name (TypeSynInfo s))
  }
```

`DataConInfo` is particularly important for GADTs:

```haskell
data DataConInfo s = DataConInfo
  { dciName        :: !Name
  , dciUnivTyVars  :: ![TyVar s]     -- universally quantified
  , dciExTyVars    :: ![TyVar s]     -- existentially quantified
  , dciTheta       :: ![Pred s]      -- constructor constraints (given on match)
  , dciArgTys      :: ![TcType s]    -- field types
  , dciResTy       :: !(TcType s)    -- result type (may mention univs)
  , dciOrigResTy   :: !(TcType s)    -- original result type from decl
  }
```

### 7.2 Local environment

```haskell
data TcEnv s = TcEnv
  { tcEnvTerms     :: !(Map Name (TcBinder s))
  , tcEnvTypes     :: !(Map Name (TcType s))  -- type variables in scope
  , tcEnvGivenEvs  :: ![EvVar]                -- given evidence in scope
  , tcEnvTcLevel   :: !TcLevel                -- current implication depth
  }

data TcBinder s
  = TcId Name (TypeScheme s)      -- polymorphic binding
  | TcMonoId Name (TcType s)      -- monomorphic binding (lambda, pattern)
```

---

## 8. The TcM monad

```haskell
newtype TcM s a = TcM (ReaderT (TcEnv s) (StateT (TcState s) (ST s)) a)

data TcState s = TcState
  { tcsUniques    :: !UniqueSupply
  , tcsEvBinds    :: !(EvBindMap s)         -- evidence bindings accumulated
  , tcsMetaVars   :: !(Map Unique (MetaRef s))  -- all created meta vars
  , tcsDiagnostics :: ![TcDiagnostic]       -- errors and warnings
  , tcsAnnotations :: ![TcNodeAnnotation]   -- collected annotations
  }
```

Using `ST s` with `STRef s`-backed meta-variable cells gives the solver
efficient mutable unification variables while keeping the overall entry
point pure (`runST`). The `s` parameter propagates through all internal
types that transitively contain a `MetaRef s`, but it is confined to the
TC internals — output types (`TcAnnotation`, `TcResult`) are `s`-free
because zonking resolves all meta-variables before results are returned.

### 8.1 Key monadic operations

```haskell
-- Fresh names and variables
newUnique     :: TcM s Unique
newMetaTv     :: TcType s -> TcM s (TyVar s)       -- fresh unification variable
newSkolemTv   :: Name -> TcType s -> TcM s (TyVar s)  -- fresh rigid variable
newEvVar      :: Pred s -> TcM s EvVar

-- Environment
lookupTerm    :: Name -> TcM s (TcBinder s)
extendTermEnv :: Name -> TcBinder s -> TcM s a -> TcM s a
extendTyEnv   :: Name -> TcType s -> TcM s a -> TcM s a
getTcLevel    :: TcM s TcLevel
pushTcLevel   :: TcM s a -> TcM s a

-- Evidence
bindEvidence  :: EvVar -> EvTerm s -> TcM s ()
lookupEvidence :: EvVar -> TcM s (Maybe (EvTerm s))

-- Annotations
annotateNode  :: HasSourceSpan n => n -> TcAnnotation -> TcM s ()

-- Errors
emitError     :: SourceSpan -> TcErrorKind -> TcM s ()
emitWarning   :: SourceSpan -> TcWarningKind -> TcM s ()
```

---

## 9. Module structure

```
Aihc/
    Tc.hs                    -- entry point: typecheck :: [Module] -> TcResult
  Tc/
    Types.hs               -- TcType, TyVar, Pred, TypeScheme, etc.
    Evidence.hs            -- EvTerm, Coercion, EvBinding
    Constraint.hs          -- Ct, Implication, CtFlavor, CtOrigin
    Monad.hs               -- TcM, TcState, TcEnv
    Env.hs                 -- GlobalEnv, DataConInfo, ClassInfo, etc.
    Annotations.hs         -- TcAnnotation, TcNodeAnnotation, pattern synonyms

    Generate.hs            -- constraint generation (bidirectional)
    Generate/
      Expr.hs              -- inferExpr, checkExpr
      Pattern.hs           -- checkPattern, inferPattern
      Decl.hs              -- tcTopBind, tcLocalLet
      Type.hs              -- kind checking, surface-to-TcType

    Solve.hs               -- top-level solver entry point
    Solve/
      Worklist.hs          -- WorkList data type and operations
      InertSet.hs          -- InertSet data type and operations
      Canonicalize.hs      -- constraint canonicalization
      Interact.hs          -- interaction rules (inert + work item)
      Equality.hs          -- equality solver (unification, decomposition)
      Dict.hs              -- class constraint solver (instance lookup)
      Implication.hs       -- implication solver
      Flatten.hs           -- type family flattening
      Defaulting.hs        -- ambiguity resolution and defaulting

    Zonk.hs                -- zonking: replace meta vars with solutions
    Generalize.hs          -- let-generalization
    Unify.hs               -- unification (meta variable solving)
    Instantiate.hs         -- scheme instantiation
    Subsumption.hs         -- subsumption checking (for higher-rank)
    Error.hs               -- error message construction
    Error/
      Messages.hs          -- user-facing error formatting
      Origins.hs           -- CtOrigin rendering
```

---

## 10. Algorithm outline

### 10.1 Top-level entry

```haskell
-- Pure entry point; ST is internal
typecheck :: [Module] -> TcResult

data TcResult = TcResult
  { tcResultModule      :: !Module          -- annotated AST
  , tcResultAnnotations :: ![TcNodeAnnotation]
  , tcResultDiagnostics :: ![TcDiagnostic]
  }
```

Internally `typecheck` runs `runST $ do ...`, building the `GlobalEnv s`
from the surface declarations inside the ST computation. The `s` parameter
never appears in the public API.

1. Build `GlobalEnv s` from module declarations (data types, classes,
   instances, type families).
2. Kind-check all type declarations.
3. Type-check each top-level binding group (respecting dependency order):
   a. Generate constraints (producing wanted constraints and evidence
      holes).
   b. Solve wanted constraints.
   c. Generalize residual constraints into type schemes.
   d. Zonk meta-variables.
   e. Attach `TcAnnotation` to AST nodes.
4. Collect and report unsolved constraints as errors.

### 10.2 Constraint generation (bidirectional)

```haskell
inferExpr :: Expr -> TcM s (Expr, TcType s, [Ct s])
checkExpr :: Expr -> TcType s -> TcM s (Expr, [Ct s])
```

The generator walks the AST and returns:

- The (possibly re-annotated) AST node
- The inferred type (for infer mode)
- A list of wanted constraints

Key rules:

- **Variable**: look up scheme, instantiate, emit wanted constraints for
  the scheme's predicates, record type applications.
- **Application**: infer function type, check argument, emit equality
  between function domain and argument type.
- **Lambda**: fresh meta for argument, extend env, infer body.
- **Let**: for unannotated local lets, infer monomorphically. For
  annotated lets, check against signature. For top-level, generalize.
- **Case**: infer scrutinee, for each branch introduce GADT givens as an
  `Implication`.
- **Type annotation**: check expression against the given type.

### 10.3 GADT branch handling

For each case alternative matching a GADT constructor:

1. Look up `DataConInfo`.
2. Instantiate universal type variables with fresh metas.
3. Introduce existential type variables as fresh skolems.
4. Emit equality between scrutinee type and instantiated result type.
5. Add constructor constraints (`dciTheta`) as givens.
6. Type-check branch body under these givens.
7. Package all branch-local wanted constraints into an `Implication`.

This is the core of OutsideIn(X): branch-local reasoning is scoped inside
implications and cannot leak to the outer level.

### 10.4 Solver

The solver uses the worklist/inert-set architecture:

```
while worklist is non-empty:
  pop constraint from worklist
  canonicalize it
  interact with inert set
  either:
    - solve it (fill evidence)
    - add to inert set
    - emit new work items
```

#### Canonicalization

- Orient equalities: meta on left, skolem on right.
- Flatten type family applications: `F a` becomes `beta` with wanted
  `F a ~ beta`.
- Normalize class predicates.

#### Equality solving

- Unify touchable meta variables (respecting `TcLevel`).
- Decompose type constructor equalities:
  `T a1 a2 ~ T b1 b2` becomes `a1 ~ b1, a2 ~ b2`.
- Apply type family axioms.
- Build coercion evidence for each step.

#### Dictionary solving

- Match wanted against given dictionaries.
- Match against instance declarations, emitting sub-goals.
- Construct evidence dictionaries.

#### Implication solving

- Enter new solver scope.
- Extend environment with skolems and givens.
- Solve inner wanteds.
- Enforce untouchability: inner branches cannot unify outer-level
  meta-variables using local-only information.
- Return residual unsolved constraints.

### 10.5 Generalization

For top-level bindings:

1. Solve all wanted constraints as far as possible.
2. Identify free meta-variables not in the environment.
3. Quantify over them.
4. Abstract over residual class constraints as dictionary parameters.
5. Record the resulting `TypeScheme` and the abstraction (type lambdas +
   dictionary lambdas) in the annotation.

For local `let` without a type signature: monomorphic (no generalization)
per OutsideIn(X) recommendation.

For local `let` with a type signature: check against the signature, using
an implication to scope the signature's quantified variables.

### 10.6 Defaulting

`Solve/Defaulting.hs` applies the Haskell 2010 section 4.3.4 rule to the
constraints that survive the solver. A meta-variable is a candidate when the
enclosing binding does not generalize over it and the environment does not
mention it. A binding with a signature makes every one of its type variables
rigid, so each meta-variable that survives is a candidate.

A candidate `v` defaults only when:

1. Every unsolved constraint that mentions `v` has the form `C v`. A
   constraint such as `C [v]`, a multi-parameter constraint, or an unsolved
   equality blocks defaulting.
2. At least one `C` is a numeric class: `Num`, `Real`, `Integral`,
   `Fractional`, `Floating`, `RealFrac`, or `RealFloat`.
3. Every `C` is a standard class. A user class blocks defaulting, so
   defaulting never picks an instance the program did not ask for.

The solver then takes the first type of the default list that is an instance
of every class in the group, and writes it as the solution of `v`. The
instance trial runs the real dictionary solver under `tcSpeculate`, which
discards the evidence, the meta-variable solutions, and the diagnostics of a
failed trial.

The default list comes from the module `default` declaration. A module
without one uses `(Integer, Double)`. `default ()` gives an empty list and
turns defaulting off.

`ExtendedDefaultRules` is not implemented.

### 10.7 Zonking

After solving, replace all meta-variables with their solutions throughout
the type annotations. Any remaining unsolved meta-variables become
ambiguity errors or are defaulted.

---

## 11. Incremental implementation plan

Build in this order, each stage adding to the previous:

### Stage 1: Monomorphic lambda calculus

- `TcType` with `TcTyVar`, `TcTyCon`, `TcFunTy` only.
- Unification of meta-variables.
- Constraint generation for: variables, application, lambda, let, literals,
  if-then-else.
- Simple worklist solver (equalities only).
- Zonking.
- Annotations on expressions with inferred types.
- **Test**: infer types for simple programs without polymorphism.

### Stage 2: Hindley-Milner polymorphism

- Add `TcForAllTy`.
- `TypeScheme`, instantiation, generalization.
- Top-level let-generalization.
- **Test**: infer polymorphic types, check against signatures.

### Stage 3: Type classes

- Add `Pred` (class predicates only).
- `TcQualTy`, `EvTerm`, `EvVar`, `EvBinding`.
- Dictionary solver: instance lookup, superclass selection.
- Evidence annotations on nodes.
- **Test**: resolve `Eq`, `Show`, `Num` constraints; multi-parameter type
  classes.

### Stage 4: Equality constraints and GADTs

- Add `EqPred`.
- `Coercion` type.
- `Implication` constraints.
- GADT pattern matching: skolems, given equalities, branch implications.
- Implication solver with untouchability.
- Coercion evidence in annotations.
- **Test**: GADT pattern matching with refinement, existentials.

### Stage 5: Type families

- `FamAxiom` representation.
- Flattening in canonicalization.
- Axiom-based equality solving.
- **Test**: open and closed type families, family + GADT interaction.

### Stage 6: Polish

- Defaulting and ambiguity resolution. Done, less `ExtendedDefaultRules`.
  See section 10.6.
- Higher-rank polymorphism (subsumption).
- Error message quality.
- Performance.

---

## 12. Testing strategy

### 12.1 Annotated golden tests

Place fixtures in `components/aihc-tc/test/Test/Fixtures/annotated/`.

Each fixture is a YAML file containing one or more Haskell modules. The
expected output is the source overlaid with annotated type information for each
binding and sub-expression, rendered in a deterministic text format.

**Format**: the `annotated` field contains one rendered overlay per module:

```yaml
extensions: []
modules:
  - |
    module Test where
    id x = x
annotated:
  - |
    module Test where
    id x = x
    │  └─ x :: a
    └─ id :: forall a. a -> a
status: pass
reason: ""
```

Fixtures should cover:

- Simple monomorphic functions
- Polymorphic functions (inferred and annotated)
- Type class usage (dictionary passing)
- GADT pattern matching (coercion evidence)
- Type family applications (axiom coercions)
- Let-generalization vs. monomorphism
- Error cases (type mismatches, ambiguity, untouchable violations)

### 12.2 Oracle tests

Use GHC as the oracle, similar to `aihc-parser`'s oracle suite.

For each test case:

1. Parse and type-check with `aihc-tc`.
2. Compile with GHC (using `-ddump-types` or `-ddump-tc-trace`).
3. Compare:
   - Does aihc-tc accept/reject the same programs as GHC?
   - Do inferred type signatures match?

**Outcome model** (same as parser):

| aihc-tc | GHC     | Status |
|---------|---------|--------|
| accept  | accept  | PASS (if types match) |
| reject  | reject  | PASS   |
| accept  | reject  | XPASS  |
| reject  | accept  | FAIL   |
| accept  | accept  | FAIL (if types differ) |

### 12.3 Property tests (Hedgehog)

- **Zonking idempotence**: zonking a fully-zonked type is a no-op.
- **Instantiation/generalization roundtrip**: generalizing an instantiated
  scheme produces an alpha-equivalent scheme.
- **Coercion well-typedness**: every constructed coercion `co : t1 ~ t2`
  has matching endpoints.
- **Solver completeness**: for generated constraint sets from well-typed
  programs, the solver should produce no residual unsolved wanted
  constraints.
- **Evidence well-formedness**: every filled evidence hole has the correct
  predicate type.

### 12.4 Error-message tests

Separate golden fixtures for error cases, capturing the full diagnostic
output. This ensures error messages remain stable and helpful.

### 12.5 Stackage progress tracking

Like the parser, track what percentage of Stackage packages can be
type-checked successfully. This gives a continuous measure of real-world
coverage.

---

## 13. Package structure

```
components/aihc-tc/
  aihc-tc.cabal
  src/
    Aihc/
      Tc.hs
      Tc/
        Types.hs
        Evidence.hs
        Constraint.hs
        Monad.hs
        Env.hs
        Annotations.hs
        Generate.hs
        Generate/
          Expr.hs
          Pattern.hs
          Decl.hs
          Type.hs
        Solve.hs
        Solve/
          Worklist.hs
          InertSet.hs
          Canonicalize.hs
          Interact.hs
          Equality.hs
          Dict.hs
          Implication.hs
          Flatten.hs
          Defaulting.hs
        Zonk.hs
        Generalize.hs
        Unify.hs
        Instantiate.hs
        Subsumption.hs
        Error.hs
        Error/
          Messages.hs
          Origins.hs
  test/
    Spec.hs
    Test/
      Tc/
        Suite.hs        -- golden test runner
        Oracle.hs       -- GHC oracle comparison
        Properties.hs   -- Hedgehog properties
      Fixtures/
        golden/         -- .hs input files + .expected output
        oracle/         -- oracle test fixtures
```

### 13.1 Dependencies

```
aihc-tc
  depends on:
    aihc-parser    (surface AST, SourceSpan, Annotation)
    aihc-resolve   (ResolvedName, for linking names to definitions)
    containers
    text
    mtl            (or transformers)
```

The type checker does *not* depend on parser CLI tooling.

---

## 14. Key design decisions

### 14.1 Annotate, don't transform

The TC does not produce a new AST type. It annotates the existing surface
AST. This keeps the interface surface small and avoids a large duplication
of the AST definition. The downstream desugarer reads annotations to build
core.

### 14.2 Evidence is real from day one

Every wanted constraint gets an evidence variable. The solver fills it.
Annotations record the binding. This is essential for the desugarer to
produce dictionary-passing and coercion-carrying core.

### 14.3 Mutable meta-variables via ST

Using `STRef`-backed meta-variables gives the solver efficient in-place
mutation for unification while keeping the public API pure (`runST`). The
`s` parameter propagates through all internal types (`TcType s`, `Pred s`,
`Ct s`, `TcEnv s`, etc.) but is confined to the TC internals — output
types are `s`-free after zonking.

### 14.4 Separate constraint generation and solving

Constraint generation walks the AST and accumulates wanted constraints.
The solver is a separate subsystem. This matches the OutsideIn(X) paper's
architecture and keeps the two concerns cleanly separated.

### 14.5 TcLevel for untouchability

Each implication has a `TcLevel`. Meta-variables created at level N cannot
be unified by the solver when processing constraints at level N+1 (unless
the solution involves only types visible at level N). This enforces the
OutsideIn discipline.

### 14.6 Monomorphic local lets by default

Unannotated local `let` bindings are not generalized, following
OutsideIn(X). This simplifies the implementation and matches GHC behavior
with `MonoLocalBinds` (which is implied by `GADTs` and `TypeFamilies`).

---

## 15. Practical simplifications for v1

- Only nominal equality (no representational/phantom).
- Only top-level instances (no local instances).
- No overlapping instances.
- No functional dependencies initially.
- No quantified constraints.
- Implicit parameters (`ImplicitParams`) are constraints. A `?x` use wants `?x` at a fresh type. A `let ?x = e` binding or a signature context supplies it. The name selects the binding, and the innermost binding wins. A `?callStack :: CallStack` constraint follows the GHC `HasCallStack` rules: an occurrence of a function with the constraint pushes its call site, and an unsolved call stack is empty.
- Eager flattening of all type family applications.
- No kind polymorphism initially (monomorphic kinds, `Type` only).
- No type-level literals initially.

These can all be added incrementally without restructuring the core
architecture.
