#!/usr/bin/env bash
set -euo pipefail
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT
cp -R bin/aihc/test/Test/Fixtures/build-identity/. "$fixture"
cp bin/aihc/Setup.hs "$fixture/Setup.hs"
cd "$fixture"
printf 'packages: . dep\n' > cabal.project
build_identity() {
  cabal clean -v0
  cabal run -v0 --offline identity-fixture
}
test -z "$(build_identity)"
git init -q
git config commit.gpgsign false
git config user.name 'Identity Fixture'
git config user.email 'fixture@example.invalid'
git add src app dep identity-fixture.cabal runtime.h Setup.hs cabal.project
git commit -qm 'test: create the identity fixture'
first=$(git rev-parse HEAD)
test "$(build_identity)" = "$first"
printf '#define RUNTIME_VALUE 2\n' > runtime.h
test "$(build_identity)" = "$first"
git add runtime.h
git commit -qm 'test: change the fixture commit'
second=$(git rev-parse HEAD)
test "$first" != "$second"
test "$(build_identity)" = "$second"
executable=$(cabal list-bin -v0 identity-fixture)
cp "$executable" "$fixture/portable-identity"
cd /
test "$("$fixture/portable-identity")" = "$second"
cd "$fixture"
rm -rf .git
test -z "$(build_identity)"
printf 'Build identity fixture passed.\n'
