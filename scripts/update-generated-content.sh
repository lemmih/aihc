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
--update | --check) ;;
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

resolve_cmd="${RESOLVE_PROGRESS_CMD:-nix run .#resolve-progress}"
resolve_extension_markdown_cmd="${RESOLVE_EXTENSION_PROGRESS_CMD:-nix run .#resolve-extension-progress -- --markdown}"
tc_cmd="${TC_PROGRESS_CMD:-nix run .#tc-progress}"
core_libs_progress_cmd="${CORE_LIBS_PROGRESS_CMD:-nix run .#aihc-dev -- core-libs-progress}"
line_counts_cmd="${LINE_COUNTS_CMD:-nix run .#line-counts}"

tmpdir="$(mktemp -d)"
cleanup() {
	rm -rf "$tmpdir"
}
trap cleanup EXIT

resolve_out="$tmpdir/resolve-progress.txt"
resolve_extension_out="$tmpdir/resolve-extension-progress.md"
tc_out="$tmpdir/tc-progress.txt"
core_libs_progress_out="$tmpdir/core-libs-progress.txt"
line_counts_out="$tmpdir/line-counts.txt"

run_cmd "$resolve_cmd" >"$resolve_out"
run_cmd "$resolve_extension_markdown_cmd" | sed -n '/^# Name Resolver Extension Support Status/,$p' >"$resolve_extension_out"
run_cmd "$tc_cmd" >"$tc_out"
run_cmd "$core_libs_progress_cmd" >"$core_libs_progress_out"
run_cmd "$line_counts_cmd" >"$line_counts_out"

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

parse_core_libs_progress() {
	local infile="$1"
	local key="$2"
	awk -v key="$key" '
    $1 == key {
      implemented = $2 + 0
      total = $3 + 0
      complete = $4 + 0
    }
    END {
      if (total == "" || total <= 0) {
        exit 2
      }
      printf "%d\n%d\n%.2f\n", implemented, total, complete
    }
  ' "$infile"
}

progress_circles() {
	local complete="$1"
	awk -v complete="$complete" '
    BEGIN {
      filled = int(complete / 20)
      if (filled < 0) {
        filled = 0
      } else if (filled > 5) {
        filled = 5
      }

      for (i = 1; i <= filled; i++) {
        printf "●"
      }
      for (i = filled + 1; i <= 5; i++) {
        printf "○"
      }
    }
  '
}

resolve_vals=($(parse_progress "$resolve_out")) || {
	echo "update-generated-content.sh: could not parse resolve-progress summary (expected PASS/XFAIL/XPASS/FAIL/TOTAL/COMPLETE on stdout)." >&2
	exit 2
}
resolve_pass="${resolve_vals[0]}"
resolve_xfail="${resolve_vals[1]}"
resolve_xpass="${resolve_vals[2]}"
resolve_fail="${resolve_vals[3]}"
resolve_total="${resolve_vals[4]}"
resolve_implemented="${resolve_vals[5]}"
resolve_complete="${resolve_vals[6]}"

tc_vals=($(parse_progress "$tc_out")) || {
	echo "update-generated-content.sh: could not parse tc-progress summary (expected PASS/XFAIL/XPASS/FAIL/TOTAL/COMPLETE on stdout)." >&2
	exit 2
}
tc_total="${tc_vals[4]}"
tc_implemented="${tc_vals[5]}"
tc_complete="${tc_vals[6]}"

ghc_prim_vals=($(parse_core_libs_progress "$core_libs_progress_out" "GHC_PRIM")) || {
	echo "update-generated-content.sh: could not parse core-libs-progress output for GHC_PRIM (expected 'GHC_PRIM N M PCT' line on stdout)." >&2
	exit 2
}
ghc_prim_implemented="${ghc_prim_vals[0]}"
ghc_prim_total="${ghc_prim_vals[1]}"
ghc_prim_complete="${ghc_prim_vals[2]}"

base_vals=($(parse_core_libs_progress "$core_libs_progress_out" "BASE")) || {
	echo "update-generated-content.sh: could not parse core-libs-progress output for BASE (expected 'BASE N M PCT' line on stdout)." >&2
	exit 2
}
base_implemented="${base_vals[0]}"
base_total="${base_vals[1]}"
base_complete="${base_vals[2]}"

resolve_circles="$(progress_circles "$resolve_complete")"
tc_circles="$(progress_circles "$tc_complete")"
ghc_prim_circles="$(progress_circles "$ghc_prim_complete")"
base_circles="$(progress_circles "$base_complete")"

cat >"$tmpdir/readme-root-resolve.txt" <<EOF2
\`${resolve_implemented}/${resolve_total}\` (\`${resolve_complete}%\`) ${resolve_circles}
EOF2

cat >"$tmpdir/readme-root-tc.txt" <<EOF2
\`${tc_implemented}/${tc_total}\` (\`${tc_complete}%\`) ${tc_circles}
EOF2

cat >"$tmpdir/readme-root-ghc-prim.txt" <<EOF2
\`${ghc_prim_implemented}/${ghc_prim_total}\` (\`${ghc_prim_complete}%\`) ${ghc_prim_circles}
EOF2

cat >"$tmpdir/readme-root-base.txt" <<EOF2
\`${base_implemented}/${base_total}\` (\`${base_complete}%\`) ${base_circles}
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
  ' "$file" >"$tmp_out"

	if [ "$mode" = "--update" ]; then
		if ! cmp -s "$file" "$tmp_out"; then
			cat "$tmp_out" >"$file"
		fi
	else
		if ! cmp -s "$file" "$tmp_out"; then
			echo "Generated block out of date: ${file} (${marker})" >&2
			stale=1
		fi
	fi
}

replace_marker_inline() {
	local file="$1"
	local marker="$2"
	local content_file="$3"
	local start="<!-- AUTO-GENERATED: START ${marker} -->"
	local end="<!-- AUTO-GENERATED: END ${marker} -->"
	local tmp_out="$tmpdir/$(basename "$file").${marker}.inline.out"

	local start_count
	local end_count
	start_count="$(grep -Foc "$start" "$file" || true)"
	end_count="$(grep -Foc "$end" "$file" || true)"
	if [ "$start_count" -ne 1 ] || [ "$end_count" -ne 1 ]; then
		echo "Expected exactly one inline marker pair for '${marker}' in ${file}" >&2
		exit 1
	fi

	local content
	content="$(tr -d '\n' <"$content_file")"

	awk -v start="$start" -v end="$end" -v content="$content" '
    {
      s = index($0, start)
      e = index($0, end)
      if (s > 0 && e > s) {
        prefix = substr($0, 1, s + length(start) - 1)
        suffix = substr($0, e)
        print prefix " " content " " suffix
      } else {
        print
      }
    }
  ' "$file" >"$tmp_out"

	if [ "$mode" = "--update" ]; then
		if ! cmp -s "$file" "$tmp_out"; then
			cat "$tmp_out" >"$file"
		fi
	else
		if ! cmp -s "$file" "$tmp_out"; then
			echo "Generated inline block out of date: ${file} (${marker})" >&2
			stale=1
		fi
	fi
}

remove_obsolete_marker_line() {
	local file="$1"
	local marker="$2"

	if [ "$mode" = "--update" ]; then
		local tmp_out="$tmpdir/$(basename "$file").${marker}.remove.out"
		grep -Fv "<!-- AUTO-GENERATED: START ${marker} -->" "$file" >"$tmp_out"
		if ! cmp -s "$file" "$tmp_out"; then
			cp "$tmp_out" "$file"
		fi
	fi
}

stale=0

if [ "$mode" = "--update" ]; then
	cp "$resolve_extension_out" docs/aihc-resolve-supported-extensions.md
else
	if ! cmp -s docs/aihc-resolve-supported-extensions.md "$resolve_extension_out"; then
		echo "Generated file out of date: docs/aihc-resolve-supported-extensions.md" >&2
		stale=1
	fi
fi

replace_marker_inline README.md "tc-progress" "$tmpdir/readme-root-tc.txt"
replace_marker_inline README.md "resolve-progress" "$tmpdir/readme-root-resolve.txt"
replace_marker_inline README.md "ghc-prim-progress" "$tmpdir/readme-root-ghc-prim.txt"
replace_marker_inline README.md "base-progress" "$tmpdir/readme-root-base.txt"
replace_marker_block README.md "line-counts" "$line_counts_out"
remove_obsolete_marker_line README.md "tc-stackage-progress"

if [ "$mode" = "--check" ] && [ "$stale" -ne 0 ]; then
	exit 1
fi
