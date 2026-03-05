# aihc

## Parser Progress

The from-scratch parser lives in `components/haskell-parser`.

Current Haskell2010 progress:
- `5/57` syntax cases implemented (`8.77%` complete)
- status breakdown: `PASS=5`, `XFAIL=52`, `XPASS=0`, `FAIL=0`

Recompute progress with:

```bash
nix run .#parser-progress
```
