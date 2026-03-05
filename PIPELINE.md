# Compiler Pipeline

This document describes, at a high level, how the compiler transforms source text into WASM-oriented code.

## 1. Parser

The parser transforms source text into a `HaskellModule` AST.

## 2. Name resolution

Name resolution annotates the `HaskellModule` AST with information about where identifiers are defined.

## 3. Type checker

The type checker removes type signatures and makes type variables explicit.

## 4. Desugarer

The desugarer removes unnecessary syntax.

The simplified code has the same semantics and is still non-strict.

## 5. Final lowering steps

The final lowering steps make high-level features explicit.

- **Strictness**: Haskell's laziness can be made explicit by using `eval`/`apply`.
- **Exceptions**: `throw`/`catch` can be replaced by explicit return values.
- **Garbage collection**: Marking roots can be made explicit by using a shadow map (this is not ideal, though).
