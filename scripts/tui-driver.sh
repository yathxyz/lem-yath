#!/usr/bin/env bash
# Sourceable tmux driver for testing Lem's TUI.
# Conventions: one tmux session per test, 200x50 pane, all output via capture-pane.

LEM_BIN="${LEM_BIN:-$(dirname "${BASH_SOURCE[0]}")/../result-lem/bin/lem}"

lem_start() { # lem_start <session> [lem-args...]
  local s="$1"; shift
  tmux kill-session -t "$s" 2>/dev/null
  tmux new-session -d -s "$s" -x 200 -y 50 "$LEM_BIN $*"
}

lem_keys() { # lem_keys <session> <tmux-send-keys args...>
  local s="$1"; shift
  tmux send-keys -t "$s" "$@"
}

lem_capture() { # lem_capture <session>
  tmux capture-pane -t "$1" -p
}

lem_wait_for() { # lem_wait_for <session> <grep-ERE> [timeout-sec=10]
  local s="$1" pat="$2" timeout="${3:-10}" i=0
  while (( i < timeout * 4 )); do
    if lem_capture "$s" | grep -qE "$pat"; then return 0; fi
    sleep 0.25; i=$((i + 1))
  done
  echo "TIMEOUT waiting for /$pat/ in session $s; screen was:" >&2
  lem_capture "$s" >&2
  return 1
}

lem_stop() { # lem_stop <session>
  tmux kill-session -t "$1" 2>/dev/null
}
