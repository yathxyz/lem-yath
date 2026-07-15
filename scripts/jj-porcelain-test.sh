#!/usr/bin/env bash
# Real-TUI acceptance for the focused Majutsu-compatible jj porcelain.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$here/scripts/tui-driver.sh"

id="${LEM_YATH_CHECK_ID:-jj-porcelain-$$}"
root="$(mktemp -d "${TMPDIR:-/tmp}/lem-yath-jj-porcelain.XXXXXX")"
session="lem-yath-jj-porcelain-$id"

cleanup() {
  lem_stop "$session" || true
  rm -rf -- "$root"
}
trap cleanup EXIT
trap 'exit 130' INT TERM

export HOME="$root/home"
export XDG_CACHE_HOME="$root/cache"
export WORKDIR="$root/work"
export JJ_CONFIG="$root/jj-config.toml"
export JJ_PAGER=cat
export NO_COLOR=1
export LEM_YATH_JJ_PORCELAIN_REPORT="$root/report"
export LEM_YATH_JJ_PORCELAIN_ROOT="$root/repository jj;safe/"

mkdir -p "$HOME" "$XDG_CACHE_HOME" "$WORKDIR" \
  "$LEM_YATH_JJ_PORCELAIN_ROOT"
: >"$LEM_YATH_JJ_PORCELAIN_REPORT"
printf '%s\n' \
  'user.name = "Lem Yath Test"' \
  'user.email = "lem-yath-test@example.invalid"' \
  >"$JJ_CONFIG"

jj_bin="$(command -v jj 2>/dev/null || true)"
if [ -z "$jj_bin" ] || [ ! -x "$jj_bin" ]; then
  echo 'jj porcelain test requires jj on PATH' >&2
  exit 1
fi

"$jj_bin" git init "$LEM_YATH_JJ_PORCELAIN_ROOT" >/dev/null
printf 'tracked through every Jujutsu operation\n' \
  >"${LEM_YATH_JJ_PORCELAIN_ROOT}working copy.txt"
"$jj_bin" -R "$LEM_YATH_JJ_PORCELAIN_ROOT" describe \
  --message $'base\nbody line' >/dev/null
base_change_id=$("$jj_bin" -R "$LEM_YATH_JJ_PORCELAIN_ROOT" log \
  --no-graph -r @ --template change_id)
"$jj_bin" -R "$LEM_YATH_JJ_PORCELAIN_ROOT" new \
  --message current >/dev/null

current_description() {
  "$jj_bin" -R "$LEM_YATH_JJ_PORCELAIN_ROOT" log --no-graph -r @ \
    --template 'description.first_line()'
}

visible_description() {
  "$jj_bin" -R "$LEM_YATH_JJ_PORCELAIN_ROOT" log --no-graph -r 'all()' \
    --template 'description.first_line() ++ "\n"' | grep -Fxq -- "$1"
}

full_description() {
  "$jj_bin" -R "$LEM_YATH_JJ_PORCELAIN_ROOT" log --no-graph \
    -r "$1" --template description
}

wait_description() {
  local expected=$1 index=0
  while ((index < 80)); do
    if [ "$(current_description)" = "$expected" ]; then
      return 0
    fi
    sleep 0.25
    index=$((index + 1))
  done
  return 1
}

report_count() {
  grep -c '^STATE ' "$LEM_YATH_JJ_PORCELAIN_REPORT" 2>/dev/null || true
}

invoke_report() {
  local before
  before=$(report_count)
  lem_keys "$session" F1
  local index=0
  while ((index < 80)); do
    if (( $(report_count) > before )); then
      return 0
    fi
    sleep 0.25
    index=$((index + 1))
  done
  return 1
}

latest_report() {
  grep '^STATE ' "$LEM_YATH_JJ_PORCELAIN_REPORT" | tail -n 1
}

replace_prompt_text() {
  local text=$1
  lem_keys "$session" C-a C-k
  tmux_cmd send-keys -t "$session" -l "$text"
  lem_keys "$session" Enter
}

failed=0
pass() { printf 'PASS  %-26s %s\n' "$1" "$2"; }
fail() {
  failed=1
  printf 'FAIL  %-26s %s\n' "$1" "$2"
  lem_capture "$session" 2>/dev/null || true
  sed -n '1,120p' "$LEM_YATH_JJ_PORCELAIN_REPORT" 2>/dev/null || true
}

fixture="$(lem-yath_lisp_string "$here/scripts/jj-porcelain-fixture.lisp")"
lem_start "$session" \
  "${LEM_YATH_JJ_PORCELAIN_ROOT}working copy.txt" \
  --eval "(load #P$fixture)"

if lem_wait_for "$session" 'NORMAL' 60 >/dev/null; then
  pass boot 'configured Lem opened the jj fixture'
else
  fail boot 'configured Lem did not reach Normal state'
fi

lem_keys "$session" Space g J
if lem_wait_for "$session" 'History' 30 >/dev/null; then
  pass open 'SPC g J opened the row-aware jj history'
else
  fail open 'the jj history porcelain did not render'
fi

lem_keys "$session" C-j
if invoke_report &&
   [[ $(latest_report) == \
     'STATE kind=log row=yes description=current rows=3 root=yes read-only=yes mode=yes keys=yes source=no source-live=yes' ]]; then
  pass navigation 'C-j selected @ and all Majutsu-compatible keys are active'
else
  fail navigation 'revision metadata, navigation, or keymap state diverged'
fi

lem_keys "$session" '?'
if lem_wait_for "$session" 'c describe' 10 >/dev/null; then
  pass help '? exposed the focused porcelain command surface'
else
  fail help 'the porcelain help summary was not visible'
fi

lem_keys "$session" c
if lem_wait_for "$session" 'Description:' 10 >/dev/null; then
  replace_prompt_text 'described in Lem'
fi
if wait_description 'described in Lem' && invoke_report &&
   [[ $(latest_report) == *'kind=log row=yes description=described_in_Lem '* ]]; then
  pass describe 'c changed the selected description and retained its row'
else
  fail describe 'description mutation or point restoration failed'
fi

lem_keys "$session" o
if lem_wait_for "$session" 'New change description' 10 >/dev/null; then
  tmux_cmd send-keys -t "$session" -l 'created in Lem'
  lem_keys "$session" Enter
fi
if wait_description 'created in Lem' && visible_description 'described in Lem'; then
  pass new 'o created and checked out a child of the selected change'
else
  fail new 'new-change creation did not preserve the selected parent'
fi

lem_keys "$session" u
if wait_description 'described in Lem' &&
   ! visible_description 'created in Lem'; then
  pass undo 'u reversed the new-change operation'
else
  fail undo 'Jujutsu operation undo did not restore the parent'
fi

lem_keys "$session" C-r
if wait_description 'created in Lem' && visible_description 'created in Lem'; then
  pass redo 'C-r restored the undone operation'
else
  fail redo 'Jujutsu operation redo did not restore the child'
fi

# Refresh preserves the old parent row; C-k reaches the newly restored child.
lem_keys "$session" C-k
if invoke_report &&
   [[ $(latest_report) == *'kind=log row=yes description=created_in_Lem '* ]]; then
  pass previous-row 'C-k moved to the preceding revision row'
else
  fail previous-row 'C-k did not select the restored child revision'
fi

lem_keys "$session" x
if lem_wait_for "$session" 'Abandon Jujutsu revision' 10 >/dev/null; then
  lem_keys "$session" y
fi
if wait_description '' &&
   ! visible_description 'created in Lem' &&
   visible_description 'described in Lem'; then
  pass abandon 'x confirmed the child removal and jj created a fresh empty @'
else
  fail abandon 'confirmed abandon did not remove the selected child'
fi

# Abandon resets the view because the selected ID disappeared.  The first row
# is jj's fresh empty @ and the second is the retained described parent.
lem_keys "$session" C-j C-j d
if lem_wait_for "$session" 'Commit ID:' 20 >/dev/null && invoke_report &&
   [[ $(latest_report) == \
     'STATE kind=show row=no description=described_in_Lem rows=0 root=yes read-only=yes mode=yes keys=yes source=no source-live=yes' ]]; then
  pass show 'd opened the selected change in a read-only jj show view'
else
  fail show 'change browsing did not open the selected revision'
fi

lem_keys "$session" q
if invoke_report && [[ $(latest_report) == *'STATE kind=log '* ]]; then
  pass show-quit 'q returned from the change view to the history'
else
  fail show-quit 'q did not restore the history buffer'
fi

# The history point is still on the described parent; C-j selects its base.
lem_keys "$session" C-j
if invoke_report &&
   [[ $(latest_report) == *'kind=log row=yes description=base\nbody_line '* ]]; then
  lem_keys "$session" c
fi
if lem_wait_for "$session" 'multiline description' 10 >/dev/null &&
   [ "$(full_description "$base_change_id")" = $'base\nbody line' ]; then
  pass multiline-refusal 'c preserved an existing multiline description'
else
  fail multiline-refusal 'the single-line prompt did not fail closed'
fi

lem_keys "$session" e
if wait_description base; then
  pass edit 'e moved the working copy to the selected historical change'
else
  fail edit 'the row-aware edit command selected the wrong revision'
fi

lem_keys "$session" q
if invoke_report &&
   [[ $(latest_report) == \
     'STATE kind=none row=no description=none rows=0 root=no read-only=no mode=no keys=yes source=yes source-live=yes' ]]; then
  pass quit 'q returned to the original live source buffer'
else
  fail quit 'the porcelain did not restore its source buffer'
fi

if ((failed)); then
  exit 1
fi
printf 'SUMMARY PASS failures=0\n'
