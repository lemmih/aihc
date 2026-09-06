#!/usr/bin/env bash
# Write a Markdown report of a cross-compilation workflow run.
#
# Usage: cross-compile-report.sh OUTPUT TARGET BUNDLES RESULTS COMPILE_LOG
#
# BUNDLES is the directory `nix build .#cross-examples-<target>` wrote, RESULTS
# the directory link-and-run-example-bundles.sh filled, and COMPILE_LOG the
# log of the nix build. Any of the three may be missing when the job that
# produces it did not finish; the report says so. The GitHub Actions variables
# GITHUB_SERVER_URL, GITHUB_REPOSITORY, GITHUB_RUN_ID, and GITHUB_SHA locate
# the run when they are set.
set -euo pipefail

if [[ $# -ne 5 ]]; then
	echo "usage: $0 OUTPUT TARGET BUNDLES RESULTS COMPILE_LOG" >&2
	exit 2
fi
output=$1
target=$2
bundles=$3
results=$4
compile_log=$5
log_lines=200

# Print FILE as a fenced block inside a collapsed section titled TITLE,
# keeping only the last lines of a long file.
render_log() {
	local title=$1 file=$2
	if [[ ! -s "$file" ]]; then
		return
	fi
	echo
	echo "<details><summary>$title</summary>"
	echo
	echo '```text'
	if (($(wc -l <"$file") > log_lines)); then
		echo "[... first lines omitted, showing the last $log_lines ...]"
	fi
	# A fence inside the log would end the block early.
	# shellcheck disable=SC2016
	tail -n "$log_lines" "$file" | sed 's/```/` ` `/g'
	echo '```'
	echo
	echo "</details>"
}

{
	echo "# Cross-compilation failure: $target"
	echo
	echo "- Date: $(date -u +%Y-%m-%d)"
	if [[ -n "${GITHUB_SHA:-}" ]]; then
		echo "- Commit: ${GITHUB_SHA}"
	fi
	if [[ -n "${GITHUB_SERVER_URL:-}" && -n "${GITHUB_REPOSITORY:-}" && -n "${GITHUB_RUN_ID:-}" ]]; then
		echo "- Run: ${GITHUB_SERVER_URL}/${GITHUB_REPOSITORY}/actions/runs/${GITHUB_RUN_ID}"
	fi
	echo "- Compile host: linux-amd64 (\`nix build .#cross-examples-$target\`)"
	echo "- Link and run host: $target"
	echo

	if [[ ! -d "$bundles" ]]; then
		echo "The compile job produced no bundles."
		render_log "Compile job log" "$compile_log"
		exit 0
	fi

	echo "| Example | Compile | Link and run |"
	echo "| --- | --- | --- |"
	for bundle in "$bundles"/*/; do
		bundle=${bundle%/}
		name=$(basename "$bundle")
		compile_status=$(<"$bundle/status")
		result="$results/$name"
		if [[ "$compile_status" != ok ]]; then
			run_status="-"
		elif [[ -f "$result/status" ]]; then
			run_status=$(head -n 1 "$result/status")
		else
			run_status="not run"
		fi
		echo "| $name | $compile_status | $run_status |"
	done

	if [[ ! -d "$results" ]]; then
		echo
		echo "The link-and-run job produced no results."
	fi

	echo
	echo "## Details"
	for bundle in "$bundles"/*/; do
		bundle=${bundle%/}
		name=$(basename "$bundle")
		compile_status=$(<"$bundle/status")
		result="$results/$name"
		if [[ "$compile_status" == ok && -f "$result/status" && "$(head -n 1 "$result/status")" == ok ]]; then
			continue
		fi
		echo
		echo "### $name"
		if [[ "$compile_status" != ok ]]; then
			render_log "Compile log" "$bundle/compile.log"
			continue
		fi
		if [[ ! -f "$result/status" ]]; then
			continue
		fi
		if (($(wc -l <"$result/status") > 1)); then
			echo
			tail -n +2 "$result/status" | sed 's/^/- /'
		fi
		render_log "Link log" "$result/link.log"
		render_log "stdout diff" "$result/stdout.diff"
		render_log "stderr diff" "$result/stderr.diff"
		if [[ "$(head -n 1 "$result/status")" == run-failed ]]; then
			render_log "stderr" "$result/stderr"
		fi
	done
} >"$output"
