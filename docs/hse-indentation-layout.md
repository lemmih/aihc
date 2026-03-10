# haskell-src-exts layout-sensitive parsing

This is a focused walkthrough of how layout (offside rule) is implemented in `haskell-src-exts`, centered on:

- `src/Language/Haskell/Exts/InternalLexer.hs`
- `src/Language/Haskell/Exts/InternalParser.ly`
- `src/Language/Haskell/Exts/ParseMonad.hs`

## Division of responsibility

Layout is implemented as a lexer+parser protocol:

- Lexer inserts virtual punctuation tokens (`SemiColon`, `VRightCurly`) at BOL.
- Parser grammar has parallel explicit-layout and implicit-layout productions (`'{' ... '}'` vs `open ... close`).
- Shared parse state carries a layout stack (`LexContext`) and two one-shot flags (`CtxtFlag`).
- There is no virtual-open token; entering implicit layout is an epsilon parser action (`open`).

Core types (`ParseMonad.hs`):

- `data LexContext = NoLayout | Layout Int`
- `type CtxtFlag = (Bool,Bool)`
  - first: do-context flag (`flagDo` / `pullDoStatus`)
  - second: force-next-token-to-be-virtual-close (`pushCtxtFlag` / `pullCtxtFlag`)

`Layout Int` is the active indentation column. `NoLayout` is pushed for explicit `{ ... }` and suppresses offside insertion while on top.

## Parser protocol: `open` / `close`

`InternalParser.ly` defines:

```haskell
open  :: { S }
  : {% pushCurrentContext >> getSrcLoc >>= \s -> return $ mkSrcSpan s s %}

close :: { S }
  : vccurly { $1 }         -- context already popped in lexer
  | error   {% popContext >> getSrcLoc >>= \s -> return $ mkSrcSpan s s %}
```

Important consequences:

- Entering an implicit layout block is parser-driven (`open` calls `pushCurrentContext`).
- Exiting can be lexer-driven (`vccurly`) or parser-recovery-driven (`close : error`).
- Context popping is intentionally split:
  - `vccurly`: lexer pops (`lexBOL` LT branch)
  - `error` close: parser pops (`popContext`)

The grammar then uses explicit and implicit forms in parallel, e.g.:

```haskell
stmtlist : '{'  stmts '}'
         | open stmts close
```

Same pattern appears for module body, decl lists, `where` blocks, case alts, etc.

## Lexer algorithm at token boundaries

`topLexer` / `lexBOL` in `InternalLexer.hs` do the offside work.

Pseudo-flow:

```text
topLexer:
  if pullCtxtFlag == True:
    emit VRightCurly immediately
  else:
    consume whitespace/comments; detect BOL
    if BOL:
      lexBOL
    else:
      lexToken

lexBOL:
  compare current_column with current_layout_indent
  LT -> setBOL; popContextL; emit VRightCurly
  EQ -> emit SemiColon
  GT -> lexToken
```

The `LT` branch is the virtual `}` insertion; `EQ` is virtual `;` insertion.
`setBOL` in the `LT` branch keeps lexer state at BOL so another virtual token may follow immediately if needed.

## How layout contexts are created

`pushCurrentContext` (`ParseMonad.hs`):

1. Read current source column (`loc`).
2. Read enclosing indent (`indent`).
3. Read+clear do-flag (`dob <- pullDoStatus`).
4. If block should be empty, set close-flag (`pushCtxtFlag`).
5. Push `Layout loc`.

Empty-block condition:

- normal: `loc <= indent`
- after flagged `do`/`mdo`: `loc < indent` (relaxed)

That relaxation is the `NondecreasingIndentation` hook:

- lexer calls `flagDo` when lexing `do`/`mdo` and extension enabled
- next `open` consumes that flag and uses the relaxed predicate once

## Explicit braces disable layout

Lexer behavior (`lexStdToken`):

- on `'{'`: `pushContextL NoLayout`
- on `'}'`: `popContextL ...`

Because `getOffside` compares against stack top, `NoLayout` blocks virtual insertion inside explicit braces.

## Tokenization examples

### Example A: `do x` / `y`

Source:

```haskell
do x
   y
```

Token stream around the block body (constructor names):

```text
KW_Do
VarId "x"
SemiColon        -- inserted: BOL column == layout column
VarId "y"
-- then either:
VRightCurly      -- if next lexing step is at BOL and offside LT
-- or no vccurly token; parser can finish via close:error at EOF edge
```

Notes:

- `open` after `do` records layout column as column of `x`.
- Second line starts in same column -> `SemiColon` from `lexBOL` EQ.
- Block termination is `lexBOL` LT (`vccurly`) when a BOL check happens at smaller indent.
- If input ends before such a BOL check, `close -> error` is the fallback.

### Example B: dedent before next token

Source:

```haskell
do
  x
z
```

Tokenization around the boundary:

```text
..., VarId "x", VRightCurly, VarId "z", ...
```

`z` is not lexed until the layout context is closed.

### Example C: explicit braces

Source:

```haskell
do { x; y }
```

Tokens:

```text
KW_Do, LeftCurly, VarId "x", SemiColon, VarId "y", RightCurly
```

No virtual punctuation is inserted inside the explicit braces.

## Parsing those tokens

Relevant productions (`InternalParser.ly`):

```haskell
expblockb : 'do' stmtlist

stmtlist  : '{'  stmts '}'
          | open stmts close

stmts     : stmt stmts1
          | ';' stmts
          | {- empty -}

stmt      : 'let' binds
          | pat '<-' trueexp
          | trueexp
          | 'rec' stmtlist
```

For `do x` / `y`, parsing is:

1. `'do' stmtlist`
2. `stmtlist -> open stmts close`
3. `stmts -> stmt stmts1`
4. first `stmt -> trueexp` (`x`)
5. `stmts1 -> ';' stmts` (consumes inserted `SemiColon`)
6. second `stmt -> trueexp` (`y`)
7. `close -> vccurly` (or `close -> error` at EOF edge)

AST shape (annotations omitted):

```haskell
Do [Qualifier (Var x), Qualifier (Var y)]
```

## Practical model

You can model hse layout as:

- parser decides where layout blocks may exist (`open ... close` sites)
- parser pushes layout columns
- lexer inserts `;` and virtual `}` from column comparisons
- parser consumes those as ordinary terminals (`';'`, `vccurly`)
- explicit `{}` bypass this via `NoLayout`

That is exactly how `do x` / `y` becomes `do { x; y }` internally.
