#!/usr/bin/env bash
set -euo pipefail
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT
cp -R bin/aihc/test/Test/Fixtures/build-identity/. "$fixture"
cp bin/aihc/Setup.hs "$fixture/Setup.hs"
cd "$fixture"
printf 'packages: . dep\n' > cabal.project
first=$(cabal run -v0 --offline identity-fixture)
second=$(cabal run -v0 --offline identity-fixture)
test "$first" = "$second"
printf 'module Unselected where\nvalue = 2\n' > src/Unselected.hs
third=$(cabal run -v0 --offline identity-fixture)
test "$first" = "$third"
sed 's/value = 1/value = 2/' src/Selected.hs > src/Selected.hs.new
mv src/Selected.hs.new src/Selected.hs
fourth=$(cabal run -v0 --offline identity-fixture)
test "$first" != "$fourth"
sed 's/value = 1/value = 2/' dep/src/Dependency.hs > dep/src/Dependency.hs.new
mv dep/src/Dependency.hs.new dep/src/Dependency.hs
fifth=$(cabal run -v0 --offline identity-fixture)
test "$fourth" != "$fifth"
sixth=$(cabal run -v0 --offline identity-fixture)
test "$fifth" = "$sixth"
printf '#define RUNTIME_VALUE 2\n' > runtime.h
seventh=$(cabal run -v0 --offline identity-fixture)
test "$sixth" != "$seventh"
eighth=$(cabal run -v0 --offline --ghc-options=-O0 identity-fixture)
test "$seventh" != "$eighth"
executable=$(cabal list-bin -v0 identity-fixture)
cp "$executable" "$fixture/portable-identity"
cd /
test "$("$fixture/portable-identity")" = "$eighth"
cd "$fixture"
shared_before=$(cabal run -v0 --offline --enable-executable-dynamic --disable-library-vanilla identity-fixture)
sed 's/value = 2/value = 3/' dep/src/Dependency.hs > dep/src/Dependency.hs.new
mv dep/src/Dependency.hs.new dep/src/Dependency.hs
shared_after=$(cabal run -v0 --offline --enable-executable-dynamic --disable-library-vanilla identity-fixture)
test "$shared_before" != "$shared_after"
printf 'Build identity fixture passed.\n'
