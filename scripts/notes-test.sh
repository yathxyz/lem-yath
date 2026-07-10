#!/usr/bin/env bash
# Hermetic regression tests for shared Org paths and capture placement.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$here/scripts/tui-driver.sh"

id="${LEM_YATH_CHECK_ID:-notes-$$}"
session="lem-yath-notes-$id"
root="$(mktemp -d "${TMPDIR:-/tmp}/lem-yath-notes.XXXXXX")"
export HOME="$root/home"
export WORKDIR="$root/work"
export PUBLIC_ORG_DIR="$root/public"
export LEM_YATH_NOTES_REPORT="$root/report"
mkdir -p "$HOME" "$WORKDIR" "$PUBLIC_ORG_DIR"

cleanup() {
  lem_stop "$session"
  rm -rf "$root"
}
trap cleanup EXIT INT TERM

test_file="$(lem-yath_lisp_string "$here/scripts/notes-test.lisp")"
lem_start_lem-yath_eval "$session" "(load #P$test_file)"
for _ in $(seq 1 240); do
  if [ -f "$LEM_YATH_NOTES_REPORT" ] &&
     grep -q '^SUMMARY ' "$LEM_YATH_NOTES_REPORT"; then
    break
  fi
  sleep 0.25
done

if [ ! -f "$LEM_YATH_NOTES_REPORT" ]; then
  echo "NOTES TEST FAILED: Lem produced no report"
  lem_capture "$session" 2>/dev/null || true
  exit 1
fi

cat "$LEM_YATH_NOTES_REPORT"
grep -q '^SUMMARY PASS ' "$LEM_YATH_NOTES_REPORT"
