# Compilation and stored artifacts

## Terms and definitions

| Term | Definition |
| --- | --- |
| Package | A named and versioned collection of modules. |
| Module | One Haskell source module with one stable package and module identity. |
| Definition identity | The identity of a top-level language entity or generated instance dictionary. |
| Name-resolution interface | The exported scope of one module. It maps each visible name to its original definition identity. |
| Type interface | The type facts that another compilation unit needs to check source code. |
| System FC artifact | The checked and desugared System FC program for one module. This document also calls it the Core artifact. |
| Object artifact | The target-specific native object file for one module. |
| Archive artifact | A native archive that contains the object artifacts for one library. |
| Compilation unit | One strongly connected component, or SCC, of the module dependency graph. |
| Dependency hash | The digest in a package artifact directory name. It identifies all direct library dependency artifact directories. |
| Incremental compilation | Compilation that lowers each module separately from System FC to an object artifact. |
| Whole-program compilation | Compilation that merges stored System FC artifacts before the GRIN pipeline. |
| Artifact store | An immutable collection of package artifact directories. |
| Static object | A native runtime object that the compiler puts directly in an object-file data section. |
| CAF | A top-level updateable thunk stored as a writable static object. |
| Static reference table | The static objects one function's code reaches without going through a heap object, plus the tables of the functions it calls directly. |

Name resolution and type checking process all modules in one compilation unit together. All later phases process each module separately.

A definition identity contains a module identity, namespace, and source name.

## Design rules

The compiler must not keep a separate whole-program cache.

Each module must have one name-resolution interface, type interface, System FC artifact, and object artifact.

The compiler must encode the first three artifacts with CBOR. The native toolchain defines the object-file format.

The compiler must generate name-resolution and type interfaces while it processes a compilation unit.

After type checking, the compiler must process each module separately. Each module must have a separate System FC artifact and object artifact.

The compiler must not store GRIN. It must generate GRIN from System FC and release it after native code generation.

The compiler must not create an object link manifest. The native object file already contains symbols, relocations, sections, and foreign references.

## Artifact store

### Package directory identity

Store each built package in this directory form:

```text
{pkg_name}-{pkg_version}-{dephash}/
```

Use a safe file-name encoding for the package name and version. Use a fixed lowercase hexadecimal encoding for `dephash`.

`dephash` must cover the full identities of all direct library dependency artifact directories.

`dephash` must not cover package source files.

Sort the dependency identities before hash calculation.

One Cabal library or executable component supplies one package build. The package plan controls its modules, dependencies, and exposed interfaces.

Each selected component identity contributes to `dephash`. Thus, one Cabal source package can produce multiple package artifact directories.

Do not change a published package directory. Build into a temporary directory, validate it, and publish it with one atomic rename.

Multiple projects can refer to the same package directory. Multiple executables can also use the same interfaces, Core artifacts, objects, and archives.

A project lock file can store package directory identities. The project must not copy package artifacts into its build directory.

### Directory layout

Use this logical layout:

```text
{pkg_name}-{pkg_version}-{dephash}/
├── package.cbor
├── modules/
│   ├── Data/
│   │   └── List/
│   │       ├── resolve.cbor
│   │       ├── type.cbor
│   │       ├── core.cbor
│   │       └── object.o
│   └── Example/
│       ├── resolve.cbor
│       ├── type.cbor
│       ├── core.cbor
│       └── object.o
└── lib/
    └── lib{pkg_name}.a
```

Map module-name components to directory components. Do not use the host path separator inside a module identity.

`package.cbor` is a store index. It is not an object link manifest.

The package index must contain:

```text
PackageIndex
  schemaVersion
  packageIdentity
  compilerIdentity
  targetTriple
  runtimeAbi
  nativeLinkInputs
  exposedModules
  archivePath

ExposedModule
  visibleModuleName
  providerModuleIdentity
```

Paths in the index must be relative to the package directory. The index must not contain temporary build paths.

`nativeLinkInputs` lists required system libraries, frameworks, and package-level linker options. It does not describe Haskell object symbols.

`exposedModules` maps each importable module name to its provider module identity. It includes Cabal module re-exports and module renames.

An executable component has an empty `exposedModules` list. A dependent component must not import a module that is absent from this list.

The package plan selects dependency libraries. Their exposed-module maps resolve source imports to package and module identities.

`archivePath` is absent when the package directory does not contain a library.

## CBOR encoding

Use deterministic CBOR for all compiler-owned stored data.

Each top-level value must start with an artifact kind. Do not put a format version in an artifact file.

Put the artifact format version in the package cache hash. A format change must change the package cache hash.

Use integer field keys or fixed-position arrays for stable records. Use explicit constructor tags for sum types.

Use definite lengths. Sort maps by their encoded keys. Use the shortest valid integer encoding.

Do not encode Haskell `Show` output. Do not depend on constructor order that the schema does not specify.

Encode text as UTF-8. Encode digests and object data as byte strings.

The schema must define every identity representation. In-memory unique numbers must not become cross-module identities.

Use these common logical identity records:

```text
PackageIdentity
  packageName
  packageVersion
  dephash

ModuleIdentity
  packageIdentity
  moduleName

DefinitionIdentity
  moduleIdentity
  namespace
  sourceName
```

`dephash` equals the dependency hash in the package directory name.

## Name-resolution interface

### Purpose

The name-resolution interface describes the exported scope of one module. Import processing uses this data without loading source or Core.

The interface includes re-exported entities. A re-export keeps the definition identity from the module that defines the entity.

The interface does not include local scopes, local unique numbers, use-site annotations, or diagnostics.

### Data format

Use this logical structure:

```text
ResolveInterface
  schemaVersion
  moduleIdentity
  sourceHash
  dependencyScopes
  scopeHash
  exports
  fixities

ResolveDependencyScope
  moduleIdentity
  scopeHash

ResolveExport
  visibleName
  namespace
  entityKind
  definitionIdentity
  parentIdentity

ResolveFixity
  definitionIdentity
  associativity
  precedence
```

`namespace` must distinguish at least terms and types. Separate namespaces permit one text name in more than one namespace.

`entityKind` must distinguish these exported entity forms:

- Value.
- Type constructor.
- Type synonym.
- Data constructor.
- Class.
- Class method.
- Record field.
- Type family.
- Data family.
- Pattern synonym, when supported.

`parentIdentity` is absent for an independent entity. It identifies the parent type, class, or pattern synonym for a bundled entity.

Constructor, method, and record-field associations support import forms such as `T(..)` and `C(method)`.

Fixity data uses the original definition identity. A re-exported operator therefore keeps its defining fixity.

An exported module item expands to its exported entities before storage. The stored interface does not need a recursive qualified-module scope.

Store entries in stable order by namespace, visible name, entity kind, and definition identity.

`sourceHash` is the hash of the Haskell module that produced the file.

`dependencyScopes` records the module identity and scope hash for each direct module dependency.

`scopeHash` covers only the exported scope in `exports` and `fixities`. It must not cover `sourceHash` or `dependencyScopes`.

Thus, the incremental input relation is:

```text
Haskell module + scopes of direct module dependencies -> resolve.cbor
```

Before reuse, read `resolve.cbor` from storage. Check its `sourceHash` against the current Haskell module.

Also check each recorded dependency scope hash against the current `scopeHash` in that dependency's `resolve.cbor` file.

Regenerate `resolve.cbor` if its Haskell module changed. Also regenerate it if a direct dependency scope changed.

A regenerated file can have a new `scopeHash`. If it does, check and possibly regenerate each direct dependent file.

A regenerated file can also keep the same `scopeHash`. In this case, do not regenerate its dependents for this change.

Treat a missing, corrupt, or unsupported cached file as a cache miss. Regenerate the file and do not fail the compilation.

### Production and use

The resolver must process all modules in one SCC together. It must use predecessor name-resolution interfaces for imports outside the SCC.

The resolver must then write one interface for each module. Each file contains only that module's exported scope.

## Type interface

### Purpose

The type interface contains the semantic facts needed to type-check dependent modules.

The interface must contain type/kind signatures for every exported symbol, even re-exports.

The interface must contain every class and family instance that this module exports. This set includes locally defined and imported instances.

The interface must also contain required support declarations. These are private local declarations referenced by an exported type or local instance.

Support declarations do not add names to the exported scope. Mark each applicable declaration as `supportOnly`.

### Data format

Use this logical structure:

```text
TypeInterface
  schemaVersion
  moduleIdentity
  terms
  typeConstructors
  dataTypes
  classes
  typeFamilies
  dataFamilies
  instances
  familyInstances
```

The following sections define the required records.

#### Terms

```text
TermInterface
  definitionIdentity
  typeScheme
```

Store one term entry for each exported value. This set includes locally defined and re-exported values.

The type scheme includes quantified variables, kinds, and constraints. Keep the original definition identity for a re-exported value.

Constructor and method types can occur in their declaration records. Do not duplicate them in `terms`.

#### Type constructors and synonyms

```text
TypeConstructorInterface
  definitionIdentity
  supportOnly
  arity
  kind
  roles
  flavor
  synonymParameters
  synonymBody
```

`flavor` distinguishes data types, newtypes, classes, type synonyms, type families, and data families.

Store `synonymParameters` and `synonymBody` only for a type synonym. Dependent type checking needs the synonym body for expansion.

#### Data types and constructors

```text
DataTypeInterface
  typeIdentity
  supportOnly
  typeVariables
  flavor
  constructors

ConstructorInterface
  definitionIdentity
  constructorIndex
  universalVariables
  existentialVariables
  constraints
  fields
  resultType
  sourceForm

ConstructorFieldInterface
  label
  fieldType
  runtimeRepresentation
  strict
  lazy
  unpack
```

`flavor` distinguishes data types and newtypes. `sourceForm` distinguishes prefix, infix, and record constructors.

`constructorIndex` gives a stable constructor order within the data type. System FC generation must preserve the constructor identity and runtime layout.

The field record contains checked types and runtime representations. The Core artifact carries these facts to the backend.

Store a constructor record when the module exports that constructor. Also store it when another stored declaration needs its semantic layout.

The name-resolution interface controls source visibility. A support constructor does not become importable through its type record.

#### Classes

```text
ClassInterface
  definitionIdentity
  supportOnly
  typeVariables
  superclasses
  methods
  defaultMethods
  defaultSignatures
  associatedFamilies

MethodInterface
  definitionIdentity
  typeScheme
```

Store method types after class type checking. Record whether each method has a default implementation.

#### Type and data families

```text
FamilyInterface
  definitionIdentity
  supportOnly
  familyKind
  parameters
  resultKind
  injectivity
  equations

FamilyEquationInterface
  axiomIdentity
  typeVariables
  leftHandSide
  rightHandSide
  role
```

Store equations that dependent type checking can use. Closed-family equation order is significant and must remain unchanged.

#### Class instances

```text
InstanceInterface
  classIdentity
  dictionaryIdentity
  typeVariables
  context
  headTypes
  overlapMode
  associatedEquations
```

Store all exported instances. This rule includes locally defined instances, imported instances, and orphan instances.

Use the dictionary identity to remove duplicate entries for the same class instance.

The dictionary identity names the System FC dictionary binding. It also gives later modules a stable reference to selected evidence.

#### Data-family instances

Open type-family equations also need a separate instance record:

```text
TypeFamilyInstanceInterface
  familyIdentity
  axiomIdentity
  typeVariables
  leftHandSide
  rightHandSide
  role
```

Store every exported open type-family equation. This set includes locally defined and imported equations.

Data-family instances use this record:

```text
DataFamilyInstanceInterface
  familyIdentity
  familyType
  typeVariables
  representationTypeIdentity
  axiomIdentity
  constructorIdentities
  isNewtype
```

Store each exported data-family instance because dependent type checking needs its representation type and equality axiom.

Use the axiom identity to remove duplicate entries for the same family instance.

### Known incomplete items

TODO: Define functional dependencies in class interfaces.

TODO: Define stable identities for default workers and dictionary support symbols.

TODO: Define the ownership of open-family equations.

TODO: Define the transitive selection rule for support declarations.

### Production and use

The type checker must process all modules in one SCC together. It must use predecessor type interfaces for declarations outside the SCC.

Load the type interface for each directly imported module.

The imported interface contains the type signatures for its re-exported names. Do not load provider interfaces for those signatures.

Thus, the incremental input relation is:

```text
Haskell module + scopes of direct module dependencies + type interfaces of direct module dependencies -> type.cbor
```

Regenerate `type.cbor` if its Haskell module changes. Also regenerate it if a direct dependency scope or type interface changes.

Hash only the semantic type interface for dependent checks. Do not include the source hash or dependency hashes in this hash.

If a regenerated semantic interface has the same hash, do not regenerate its dependents for this change.

The type checker must produce one type interface for each module. A file must not contain the combined interface for the complete SCC.

## System FC artifacts

The System FC plan is in `docs/system-fc.md`.

### Per-module form

After SCC type checking, desugar each checked module separately. Write one `core.cbor` file for each module.

Use this logical top-level structure:

```text
CoreArtifact
  schemaVersion
  moduleIdentity
  coreSchemaVersion
  topLevelDeclarations
```

`topLevelDeclarations` contains the complete System FC program for the module. It can contain these declaration forms:

- External term declarations with their types and definition identities.
- Data declarations.
- Equality axioms.
- Newtype declarations.
- Primitive declarations.
- Foreign imports.
- Non-recursive and recursive value bindings.

The CBOR schema must encode all System FC types, kinds, coercions, expressions, alternatives, literals, and binders.

Top-level references must use stable definition identities. Local binders can use module-local unique numbers.

Local unique numbers must be deterministic for equal inputs. They only need uniqueness inside one Core artifact.

The artifact must contain all external declarations needed to check, lower, and merge the module.

These declarations include external types, constructor layouts, newtype facts, and equality axioms.

After System FC generation, later phases must not load name-resolution or type interfaces.

### Compilation-unit boundary

One Core artifact contains one module, including a module from a cyclic SCC. System FC supports external references between these module artifacts.

An SCC controls only name resolution and type checking. After type checking, each module has an independent Core and backend pipeline.

### Incremental use

For incremental compilation, load one module's Core artifact. Generate GRIN, run the GRIN pipeline, and emit that module's object artifact.

Use only the semantic facts in the Core artifact. Do not reconstruct Haskell type facts in GRIN.

Release the GRIN program after object generation. Do not store a GRIN interface or GRIN cache.

### Whole-program use

For whole-program compilation, load the Core artifacts for all required modules. Merge them into one System FC program.

Start with the executable Core artifacts. Follow each external definition identity to its provider module until no new provider remains.

A module identity gives its package directory and module name. Derive its Core path from the specified directory layout.

This process does not require a reachability manifest.

The merge must resolve external declarations by definition identity. It must also verify that external and defining types agree.

The merge must combine equal external semantic declarations. It must reject declarations that disagree.

The merge must assign unique merged identities to all module-local term, type, and coercion binders.

Run whole-program System FC passes after the merge. Then generate one GRIN program and one whole-program object file.

Link this object file with the RTS and required foreign libraries. Do not link package archives into the whole-program executable.

## Object artifacts

### Contents

`object.o` uses the native object format for the selected target. Examples include ELF, Mach-O, COFF, and WebAssembly objects.

Each module object must contain:

- Native code for the module's generated entries.
- Defined native symbols for the module's top-level values.
- Undefined native symbols for referenced definitions and foreign calls.
- Native relocations for all cross-symbol references.
- Read-only info tables and other constant data.
- Writable static objects for CAFs and other mutable static values.
- Unwind or debug sections when the selected compiler options request them.

The object must use one uniform value ABI. References to external Haskell values are object addresses used through `eval` and `apply`.

Each backend defines its native symbol encoding and other target-specific object conventions.

The backend must derive each external value symbol from its definition identity. Defined and undefined symbols must use the same encoding.

Cross-module Haskell values use `eval` and `apply`. Constructor tests inspect constructor identities in the current info tables.

The object does not need constructor, primitive, reachability, defined-symbol, or required-symbol metadata in a separate file.

### Static CAFs

Emit each CAF directly into a writable data section. Put its initial info-table address and captured fields in that object.

Use native relocations for info tables and captured static values. The native linker resolves cyclic references between static objects.

Reserve at least one payload word for an updateable zero-field thunk. Evaluation uses this word for the indirection target.

Do not list the static objects in a section. The collector finds a static object by its address: a pointer outside both spaces of the managed heap names an object that never moves. The runtime records each static thunk that becomes an indirection, so an evaluated CAF that only code references stays alive.

The collector must not move a static object. When it marks one, it must scan the object with its current info table and its pointer bitmap, and update each heap pointer in the object's fields.

This design does not require a CAF initializer. It also does not require a per-module initializer function.

### Static reference tables

An evaluated CAF holds the value it produced, so treating every static object as a root keeps everything any CAF has ever produced. Static reference tables give the collector a basis for deciding which static objects are live instead. The runtime does not yet use them by default; see the RTS notes for the current state.

Emit one static reference table for each function whose code reaches a static object that needs marking. The table names the static objects the function's code mentions directly, and the tables of the functions it calls by name.

Tables are chained rather than flattened. The reachable set is the same as a transitive closure over known calls, but every reference stays an ordinary relocation and the analysis never needs the whole program.

A node the function stores on the heap contributes no entry. The stored object carries its own info table, and that table names the target function's table, so the collector reaches it exactly while the object lives.

Put a pointer to the table in the info table of every closure and thunk whose entry is that function. Constructor info tables have no table: a constructor has no code.

Compiled functions must publish their own table on entry. After CPS conversion every call is a tail call, so a running function has no heap object of its own to carry its table, and a collection can happen at one of its safepoints or inside a runtime helper it called. A function with no table must publish an empty one rather than leave behind the table of a function that has already transferred control away.

A table record is word-uniform: a mutable walk link, the object count, the child count, then the static object addresses followed by the addresses of the child tables. The link is mutable, so records belong in a writable section even though info tables are read-only.

### Native linking

Use the native linker and archive index to resolve object symbols. An undefined reference can cause extraction of the applicable archive member.

Foreign libraries and linker options come from package configuration. They do not come from an object link manifest.

## Library archives

After all module objects exist, compile C sources from the Cabal `c-sources` field. Put the module objects and the C objects in `lib/lib{pkg_name}.a`. Create the native archive symbol index.

The archive is a derived package artifact. A build can recreate it from the module objects and C objects, but storage avoids repeated archive construction.

The archive must contain separate object members. This layout lets the linker extract only required members.

An incremental executable link uses its object files, dependency archives, the RTS, and configured foreign libraries.

## Compilation workflows

### Library compilation

1. Parse every source module.
2. Build the module dependency graph.
3. Order its SCCs by their dependencies.
4. Resolve all modules in one ready SCC together.
5. Write one name-resolution interface for each SCC member.
6. Type-check all modules in that SCC together.
7. Write one type interface for each SCC member.
8. Desugar each checked module to its own System FC artifact.
9. Lower each System FC artifact through GRIN to one object artifact.
10. Release each temporary GRIN program.
11. Compile C sources from the Cabal file.
12. Build the library archive from all module objects and C objects.
13. Write the package index and publish the immutable package directory.

Independent ready SCCs can run at the same time. Modules can also run at the same time after their SCC type check completes.

### Incremental executable compilation

Compile executable modules with the same SCC rules. Generate one object artifact for each executable module.

Link these objects with dependency archives, the RTS, and required foreign libraries.

### Whole-program executable compilation

Perform the same parse, resolution, type-check, and per-module System FC steps.

Load required dependency Core artifacts from their shared package directories. Merge all required Core artifacts into one System FC program.

Run the GRIN pipeline once for the merged program. Emit one object and link it with the RTS and required foreign libraries.

Do not build or load a whole-program cache. The per-module Core artifacts are the reusable whole-program input.

## Rebuild and reuse rules

If one source file changes, resolve and type-check its complete SCC again.

After type checking, generate Core and object artifacts separately for each module.

Recompute a dependent SCC only when an interface digest that it uses changes.

A changed target changes `dephash` and invalidates all stored artifacts. The compiler must build a separate package directory for that target.

The package directory remains target-specific because it is immutable and contains target-specific files.

Two projects reuse an artifact when all hashed inputs agree. Project location and build time must not affect artifact identity.

## Data that is intentionally absent

The design does not store these items:

- A whole-program cache.
- GRIN programs.
- GRIN interfaces.
- Backend interfaces for Haskell symbols.
- Object link manifests.
- Per-module initializer lists.
- Duplicate types for re-exported declarations.
- Combined SCC Core files.

The package index records package storage and native link facts only. Compiler interfaces contain language facts, and native objects contain linker facts.
