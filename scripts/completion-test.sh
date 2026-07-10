#!/usr/bin/env bash
# Real-TUI regression tests for the Vertico/Prescient-style prompt layer.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
id="${LEM_YATH_CHECK_ID:-completion-$$}"
root="$(mktemp -d "${TMPDIR:-/tmp}/lem-yath-completion.XXXXXX")"
export HOME="$root/home"
export WORKDIR="$root/work"
export LEM_YATH_COMPLETION_STATE_FILE="$root/completion-ranking.sexp"
mkdir -p "$HOME" "$WORKDIR/roam"

source "$here/scripts/tui-driver.sh"

sessions=()
failed=0

cleanup() {
  local session
  for session in "${sessions[@]:-}"; do
    [ -n "$session" ] && lem_stop "$session"
  done
  rm -rf "$root"
}
trap cleanup EXIT INT TERM

pass() { printf 'PASS  %-28s %s\n' "$1" "$2"; }
fail() {
  failed=1
  printf 'FAIL  %-28s %s\n' "$1" "$2"
  [ -n "${3:-}" ] && lem_capture "$3" 2>/dev/null || true
}

start_session() {
  local session=$1
  sessions+=("$session")
  lem_start_lem-yath "$session"
  if ! lem_wait_for "$session" 'NORMAL|Dashboard' 40 >/dev/null; then
    fail boot "Lem did not reach its dashboard" "$session"
    return 1
  fi
  sleep 0.5
}

open_query() {
  local session=$1 query=$2
  lem_keys "$session" Escape
  sleep 0.2
  lem_keys "$session" Escape
  sleep 0.2
  lem_keys "$session" M-x
  lem_wait_for "$session" 'Command:' 10 >/dev/null || return 1
  tmux_cmd send-keys -t "$session" -l "$query"
  # The empty prompt already contains ranked candidates.  Let Lem consume the
  # literal tmux input before a candidate-based assertion can match stale rows.
  sleep 0.5
}

close_prompt() {
  lem_keys "$1" Escape
  sleep 0.2
  lem_keys "$1" Escape
  sleep 0.3
}

s1="lem-yath-completion-a-$id"
if start_session "$s1"; then
  if open_query "$s1" 'roam fi' &&
     lem_wait_for "$s1" 'lem-yath-roam-find' 10 >/dev/null; then
    pass literal-components "space-separated components keep the popup open"
  else
    fail literal-components "multi-component literal matching failed" "$s1"
  fi
  close_prompt "$s1"

  if open_query "$s1" 'roam.*find' &&
     lem_wait_for "$s1" 'lem-yath-roam-find' 10 >/dev/null; then
    pass regexp-component "a regexp component matched the command label"
  else
    fail regexp-component "regexp matching failed" "$s1"
  fi
  close_prompt "$s1"

  if open_query "$s1" 'lyrf' &&
     lem_wait_for "$s1" 'lem-yath-roam-find' 10 >/dev/null; then
    pass initialism "lyrf matched lem-yath-roam-find"
  else
    fail initialism "initialism matching failed" "$s1"
  fi
  close_prompt "$s1"

  if open_query "$s1" 'lem-yath-roam-' &&
     lem_wait_for "$s1" 'lem-yath-roam-(find|insert|random)' 10 >/dev/null; then
    lem_keys "$s1" C-n
    sleep 0.3
    lem_keys "$s1" Enter
    if lem_wait_for "$s1" 'Command: lem-yath-roam-insert' 10 >/dev/null; then
      pass control-navigation "C-n moved focus and Return inserted the candidate"
    else
      fail control-navigation "C-n/Return did not select the next candidate" "$s1"
    fi
  else
    fail control-navigation "candidate set did not open" "$s1"
  fi
  close_prompt "$s1"

  if open_query "$s1" 'lem-yath-roam-' &&
     lem_wait_for "$s1" 'lem-yath-roam-find' 10 >/dev/null; then
    lem_keys "$s1" C-p
    sleep 0.3
    lem_keys "$s1" Enter
    if lem_wait_for "$s1" 'Command: lem-yath-roam-random' 10 >/dev/null; then
      pass cyclic-navigation "C-p wrapped from the first candidate to the last"
    else
      fail cyclic-navigation "completion navigation did not wrap" "$s1"
    fi
  else
    fail cyclic-navigation "candidate set did not reopen" "$s1"
  fi
  close_prompt "$s1"

  # Execute a harmless command through the real prompt so the Return wrapper
  # records it.  WORKDIR is an empty fixture, so roam-random only emits a
  # message and cannot touch the user's notes.
  if open_query "$s1" 'lem-yath-roam-random'; then
    sleep 0.8
    lem_keys "$s1" Enter
    sleep 0.3
    lem_keys "$s1" Enter
    sleep 0.8
    if open_query "$s1" 'lem-yath-roam-' &&
       lem_wait_for "$s1" 'lem-yath-roam-random' 10 >/dev/null; then
      screen=$(lem_capture "$s1")
      random_line=$(printf '%s\n' "$screen" | grep -n -m1 'lem-yath-roam-random' | cut -d: -f1)
      find_line=$(printf '%s\n' "$screen" | grep -n -m1 'lem-yath-roam-find' | cut -d: -f1)
      if [ -n "$random_line" ] && [ -n "$find_line" ] &&
         [ "$random_line" -lt "$find_line" ]; then
        pass learned-ranking "the selected command moved ahead of shorter candidates"
      else
        fail learned-ranking "the selected candidate was not ranked first" "$s1"
      fi
    else
      fail learned-ranking "ranked candidate set did not reopen" "$s1"
    fi
  else
    fail learned-ranking "could not execute the ranking probe" "$s1"
  fi
  close_prompt "$s1"

  # Exit through Lem so the persistence hook runs.
  if open_query "$s1" 'quick-exit'; then
    sleep 0.5
    lem_keys "$s1" Enter
    sleep 0.3
    lem_keys "$s1" Enter
    for _ in $(seq 1 40); do
      tmux_cmd has-session -t "$s1" 2>/dev/null || break
      sleep 0.25
    done
  fi
  if [ -s "$LEM_YATH_COMPLETION_STATE_FILE" ]; then
    pass ranking-save "usage data was saved on clean editor exit"
  else
    fail ranking-save "clean exit did not persist completion ranking"
  fi
fi

s2="lem-yath-completion-b-$id"
if start_session "$s2"; then
  if open_query "$s2" 'lem-yath-roam-' &&
     lem_wait_for "$s2" 'lem-yath-roam-random' 10 >/dev/null; then
    screen=$(lem_capture "$s2")
    random_line=$(printf '%s\n' "$screen" | grep -n -m1 'lem-yath-roam-random' | cut -d: -f1)
    find_line=$(printf '%s\n' "$screen" | grep -n -m1 'lem-yath-roam-find' | cut -d: -f1)
    if [ -n "$random_line" ] && [ -n "$find_line" ] &&
       [ "$random_line" -lt "$find_line" ]; then
      pass ranking-restore "saved ranking survived a fresh Lem process"
    else
      fail ranking-restore "fresh process did not restore ranking" "$s2"
    fi
  else
    fail ranking-restore "fresh process did not show ranked candidates" "$s2"
  fi
fi

if [ "$failed" = 0 ]; then
  echo "COMPLETION TEST PASSED"
  exit 0
else
  echo "COMPLETION TEST FAILED"
  exit 1
fi
