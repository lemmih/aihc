#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: scripts/update-generated-content.sh [--update|--check]

  --update  Rewrite generated files/sections in place
  --check   Exit non-zero if generated files/sections are out of date
USAGE
}

if [ "$#" -ne 1 ]; then
  usage >&2
  exit 2
fi

mode="$1"
case "$mode" in
  --update|--check) ;;
  *)
    usage >&2
    exit 2
    ;;
esac

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

if [ ! -f flake.nix ]; then
  echo "Run this script from inside the repository." >&2
  exit 1
fi

run_cmd() {
  local cmd="$1"
  bash -c "$cmd"
}

parser_cmd="${PARSER_PROGRESS_CMD:-nix run .#parser-progress}"
extension_cmd="${PARSER_EXTENSION_PROGRESS_CMD:-nix run .#parser-extension-progress -- --markdown}"
name_resolution_cmd="${NAME_RESOLUTION_PROGRESS_CMD:-nix run .#name-resolution-progress}"

tmpdir="$(mktemp -d)"
cleanup() {
  rm -rf "$tmpdir"
}
trap cleanup EXIT

parser_out="$tmpdir/parser-progress.txt"
extension_out="$tmpdir/extension-progress.md"
name_out="$tmpdir/name-resolution-progress.txt"

run_cmd "$parser_cmd" > "$parser_out"
run_cmd "$extension_cmd" | sed -n '/^# Haskell Parser Extension Support Status/,$p' > "$extension_out"
run_cmd "$name_resolution_cmd" > "$name_out"

parse_progress() {
  local infile="$1"
  awk '
    /^PASS[[:space:]]+/ { pass=$2 }
    /^XFAIL[[:space:]]+/ { xfail=$2 }
    /^XPASS[[:space:]]+/ { xpass=$2 }
    /^FAIL[[:space:]]+/ { fail=$2 }
    /^TOTAL[[:space:]]+/ { total=$2 }
    /^COMPLETE[[:space:]]+/ {
      gsub(/%/, "", $2)
      complete=$2
    }
    END {
      if (total == "" || pass == "" || xfail == "" || xpass == "" || fail == "" || complete == "") {
        exit 2
      }
      implemented = pass + xpass
      printf "%d\n%d\n%d\n%d\n%d\n%d\n%.2f\n", pass, xfail, xpass, fail, total, implemented, complete
    }
  ' "$infile"
}

parse_extension_summary() {
  local infile="$1"
  awk '
    /^- Total Extensions:/ { total=$4 }
    /^- Supported:/ { supported=$3 }
    /^- In Progress:/ { in_progress=$4 }
    /^- Planned:/ { planned=$3 }
    END {
      if (total == "" || supported == "" || in_progress == "" || planned == "") {
        exit 2
      }
      printf "%d\n%d\n%d\n%d\n", total, supported, in_progress, planned
    }
  ' "$infile"
}

readarray -t parser_vals < <(parse_progress "$parser_out")
parser_pass="${parser_vals[0]}"
parser_xfail="${parser_vals[1]}"
parser_xpass="${parser_vals[2]}"
parser_fail="${parser_vals[3]}"
parser_total="${parser_vals[4]}"
parser_implemented="${parser_vals[5]}"
parser_complete="${parser_vals[6]}"

readarray -t name_vals < <(parse_progress "$name_out")
name_pass="${name_vals[0]}"
name_xfail="${name_vals[1]}"
name_xpass="${name_vals[2]}"
name_fail="${name_vals[3]}"
name_total="${name_vals[4]}"
name_implemented="${name_vals[5]}"
name_complete="${name_vals[6]}"

readarray -t ext_vals < <(parse_extension_summary "$extension_out")
ext_total="${ext_vals[0]}"
ext_supported="${ext_vals[1]}"
ext_in_progress="${ext_vals[2]}"
ext_planned="${ext_vals[3]}"

cat > "$tmpdir/readme-root-parser.txt" <<EOF2
- \`${parser_implemented}/${parser_total}\` syntax cases implemented (\`${parser_complete}%\` complete)
- status breakdown: \`PASS=${parser_pass}\`, \`XFAIL=${parser_xfail}\`, \`XPASS=${parser_xpass}\`, \`FAIL=${parser_fail}\`
EOF2

cat > "$tmpdir/readme-root-extension.txt" <<EOF2
- Total tracked extensions: \`${ext_total}\`
- Supported: \`${ext_supported}\`
- In Progress: \`${ext_in_progress}\`
- Planned: \`${ext_planned}\`
EOF2

cat > "$tmpdir/readme-root-name.txt" <<EOF2
- \`${name_implemented}/${name_total}\` capability cases implemented (\`${name_complete}%\` complete)
- status breakdown: \`PASS=${name_pass}\`, \`XFAIL=${name_xfail}\`, \`XPASS=${name_xpass}\`, \`FAIL=${name_fail}\`
EOF2

cat > "$tmpdir/readme-parser-h2010.txt" <<EOF2
- \`${parser_implemented}/${parser_total}\` implemented (\`${parser_complete}%\` complete)
- \`PASS=${parser_pass}\`, \`XFAIL=${parser_xfail}\`, \`XPASS=${parser_xpass}\`, \`FAIL=${parser_fail}\`
EOF2

cat > "$tmpdir/readme-parser-extension.txt" <<EOF2
- Total tracked extensions: \`${ext_total}\`
- Supported: \`${ext_supported}\`
- In Progress: \`${ext_in_progress}\`
- Planned: \`${ext_planned}\`
EOF2

cat > "$tmpdir/readme-name-resolution.txt" <<EOF2
- \`${name_implemented}/${name_total}\` implemented (\`${name_complete}%\` complete)
- \`PASS=${name_pass}\`, \`XFAIL=${name_xfail}\`, \`XPASS=${name_xpass}\`, \`FAIL=${name_fail}\`
EOF2

replace_marker_block() {
  local file="$1"
  local marker="$2"
  local content_file="$3"
  local start="<!-- AUTO-GENERATED: START ${marker} -->"
  local end="<!-- AUTO-GENERATED: END ${marker} -->"
  local tmp_out="$tmpdir/$(basename "$file").${marker}.out"

  local start_count
  local end_count
  start_count="$(grep -Fxc "$start" "$file" || true)"
  end_count="$(grep -Fxc "$end" "$file" || true)"
  if [ "$start_count" -ne 1 ] || [ "$end_count" -ne 1 ]; then
    echo "Expected exactly one marker pair for '${marker}' in ${file}" >&2
    exit 1
  fi

  awk -v start="$start" -v end="$end" -v content_file="$content_file" '
    $0 == start {
      print
      while ((getline line < content_file) > 0) {
        print line
      }
      close(content_file)
      in_block = 1
      next
    }
    $0 == end {
      in_block = 0
      print
      next
    }
    !in_block { print }
  ' "$file" > "$tmp_out"

  if [ "$mode" = "--update" ]; then
    if ! cmp -s "$file" "$tmp_out"; then
      cat "$tmp_out" > "$file"
    fi
  else
    if ! cmp -s "$file" "$tmp_out"; then
      echo "Generated block out of date: ${file} (${marker})" >&2
      stale=1
    fi
  fi
}

stale=0

if [ "$mode" = "--update" ]; then
  cp "$extension_out" docs/haskell-parser-extension-support.md
else
  if ! cmp -s docs/haskell-parser-extension-support.md "$extension_out"; then
    echo "Generated file out of date: docs/haskell-parser-extension-support.md" >&2
    stale=1
  fi
fi

replace_marker_block README.md "parser-progress" "$tmpdir/readme-root-parser.txt"
replace_marker_block README.md "parser-extension-progress" "$tmpdir/readme-root-extension.txt"
replace_marker_block README.md "name-resolution-progress" "$tmpdir/readme-root-name.txt"
replace_marker_block components/haskell-parser/README.md "haskell2010-progress" "$tmpdir/readme-parser-h2010.txt"
replace_marker_block components/haskell-parser/README.md "extension-progress" "$tmpdir/readme-parser-extension.txt"
replace_marker_block components/haskell-name-resolution/README.md "name-resolution-progress" "$tmpdir/readme-name-resolution.txt"

if [ "$mode" = "--check" ] && [ "$stale" -ne 0 ]; then
  exit 1
fi
