#!/usr/bin/env bash
# Native Org mode and Evil-Org parity tests through the real ncurses editor.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$here/scripts/tui-driver.sh"

id="${LEM_YATH_CHECK_ID:-org-$$}"
root="$(mktemp -d "${TMPDIR:-/tmp}/lem-yath-org.XXXXXX")"
export HOME="$root/home"
export WORKDIR="$root/work"
export LEM_YATH_ORG_REPORT="$root/report"
mkdir -p "$HOME" "$WORKDIR"

fixture="$root/work/fixture.org"
target="$root/work/target.org"
other="$root/work/other.org"
eof_fixture="$root/work/eof.org"
fixture_lisp="$(lem-yath_lisp_string "$here/scripts/org-fixture.lisp")"

write_fixture() {
  printf '%s\n' \
    '#+title: Org fixture' \
    '' \
    '* TODO Parent :work:' \
    ':PROPERTIES:' \
    ':ID: 11111111-1111-4111-8111-111111111111' \
    ':END:' \
    'SCHEDULED: <2026-07-12 Sun>' \
    'Parent body sentinel with *bold* and /italic/.' \
    '** NEXT Child' \
    'Child body sentinel.' \
    '*** Grandchild' \
    'Grand body sentinel.' \
    '* Sibling' \
    'Sibling body sentinel and [[file:target.org][target]].' \
    '' \
    '- [ ] first' \
    '- [X] second' \
    '' \
    '| name | value |' \
    '|------+-------|' \
    '| alpha| beta  |' \
    '' \
    '#+begin_src python' \
    "print('hello')" \
    '#+end_src' \
    '' \
    '  | nested| cell |' \
    '  |-------+------|' \
    '' \
    '|---+-----|' \
    >"$fixture"
  printf '%s\n' 'TARGET FILE SENTINEL' >"$target"
  printf '%s\n' '* Other' 'Other body sentinel.' >"$other"
  printf '%s' '* Tail' >"$eof_fixture"
}

SESSIONS=()
FAILED=0

cleanup() {
  for session in "${SESSIONS[@]:-}"; do
    [ -n "$session" ] && lem_stop "$session"
  done
  rm -rf "$root"
}
trap cleanup EXIT INT TERM

pass() { printf 'PASS  %-26s %s\n' "$1" "$2"; }
fail() {
  FAILED=1
  printf 'FAIL  %-26s %s\n' "$1" "$2"
  [ -n "${3:-}" ] && lem_capture "$3" 2>/dev/null || true
}

start_org() {
  local label="$1" session="lem-org-$1-$id"
  : >"$LEM_YATH_ORG_REPORT"
  SESSIONS+=("$session")
  lem_start "$session" --eval "(load #P$fixture_lisp)" "$fixture"
  if ! lem_wait_for "$session" 'Parent body sentinel' 40 >/dev/null; then
    fail "$label" "fixture did not open" "$session"
    return 1
  fi
  sleep 0.5
  tmux_cmd send-keys -t "$session" Escape
  sleep 0.25
  ORG_SESSION="$session"
}

mx() {
  local session="$1" command="$2"
  tmux_cmd send-keys -t "$session" Escape Escape M-x
  lem_wait_for "$session" 'Command:' 10 >/dev/null || return 1
  tmux_cmd send-keys -t "$session" -l "$command"
  sleep 0.5
  tmux_cmd send-keys -t "$session" Enter
  sleep 0.25
  tmux_cmd send-keys -t "$session" Enter
  sleep 0.5
}

wait_report() {
  local pattern="$1" i
  for i in $(seq 1 80); do
    grep -qE "$pattern" "$LEM_YATH_ORG_REPORT" && return 0
    sleep 0.1
  done
  return 1
}

screen_has() { lem_capture "$1" | grep -qE "$2"; }
screen_lacks() { ! lem_capture "$1" | grep -qE "$2"; }

write_fixture

# 01: real mode selection, semantic parser, prose UI, and negative Evil keys.
if start_org static; then
  mx "$ORG_SESSION" lem-yath-test-org-static-report
  if wait_report '^STATIC mode=ORG-MODE programming=no heading=DOCUMENT-HEADER1-ATTRIBUTE todo=ORG-TODO-ATTRIBUTE drawer=DOCUMENT-METADATA-ATTRIBUTE timestamp=ORG-TIMESTAMP-ATTRIBUTE table=DOCUMENT-TABLE-ATTRIBUTE link=DOCUMENT-LINK-ATTRIBUTE source=DOCUMENT-CODE-BLOCK-ATTRIBUTE$' &&
     wait_report '^KEYS tab-org=yes t-todo=no T-todo=no return-org=no c-return-org=yes cs-return-org=yes m-o-other=yes$' &&
     screen_has "$ORG_SESSION" 'Org'; then
    pass static "Org mode, semantic faces, and active Evil key-theme boundaries match"
  else
    fail static "mode, attributes, or effective keymaps differed" "$ORG_SESSION"
    cat "$LEM_YATH_ORG_REPORT"
  fi
fi

# 02: local folding, atomic visible movement, reveal, and byte preservation.
if [ -n "${ORG_SESSION:-}" ]; then
  mx "$ORG_SESSION" lem-yath-test-org-goto-parent
  tmux_cmd send-keys -t "$ORG_SESSION" Tab
  sleep 0.4
  folded_ok=0
  if screen_has "$ORG_SESSION" 'Parent.*\[\.\.\.\]' &&
     screen_lacks "$ORG_SESSION" 'Parent body sentinel' &&
     screen_lacks "$ORG_SESSION" 'Child body sentinel' &&
     screen_has "$ORG_SESSION" 'Sibling body sentinel'; then
    folded_ok=1
  fi
  tmux_cmd send-keys -t "$ORG_SESSION" 9 9 j
  sleep 0.3
  mx "$ORG_SESSION" lem-yath-test-org-point-report
  atomic_ok=0
  wait_report 'POINT .*text="\* TODO Parent :work:" hidden=no modified=no folds=1' &&
    atomic_ok=1
  tmux_cmd send-keys -t "$ORG_SESSION" j
  sleep 0.3
  mx "$ORG_SESSION" lem-yath-test-org-point-report
  visible_move_ok=0
  wait_report 'POINT .*text="\* Sibling" hidden=no modified=no folds=1' &&
    visible_move_ok=1
  mx "$ORG_SESSION" lem-yath-test-org-goto-sibling
  tmux_cmd send-keys -t "$ORG_SESSION" Tab
  sleep 0.3
  tmux_cmd send-keys -t "$ORG_SESSION" 9 9 j
  sleep 0.3
  mx "$ORG_SESSION" lem-yath-test-org-point-report
  folded_tail_ok=0
  wait_report 'POINT .*text="\* Sibling" hidden=no modified=no folds=2' &&
    folded_tail_ok=1
  tmux_cmd send-keys -t "$ORG_SESSION" Tab
  sleep 0.2
  mx "$ORG_SESSION" lem-yath-test-org-goto-grand-body
  mx "$ORG_SESSION" lem-yath-test-org-point-report
  reveal_ok=0
  if wait_report 'POINT .*text="Grand body sentinel\." hidden=no modified=no folds=0' &&
     screen_has "$ORG_SESSION" 'Grand body sentinel'; then
    reveal_ok=1
  fi
  mx "$ORG_SESSION" lem-yath-test-org-goto-parent
  tmux_cmd send-keys -t "$ORG_SESSION" Tab
  sleep 0.3
  tmux_cmd send-keys -t "$ORG_SESSION" Tab
  sleep 0.3
  children_ok=0
  if screen_has "$ORG_SESSION" '\*\* NEXT Child' &&
     screen_lacks "$ORG_SESSION" 'Parent body sentinel' &&
     screen_lacks "$ORG_SESSION" 'Grand body sentinel'; then
    children_ok=1
  fi
  tmux_cmd send-keys -t "$ORG_SESSION" Tab
  sleep 0.3
  subtree_ok=0
  screen_has "$ORG_SESSION" 'Parent body sentinel' &&
    screen_has "$ORG_SESSION" 'Grand body sentinel' && subtree_ok=1
  if [ "$folded_ok" = 1 ] && [ "$atomic_ok" = 1 ] &&
     [ "$visible_move_ok" = 1 ] && [ "$folded_tail_ok" = 1 ] &&
     [ "$reveal_ok" = 1 ] && [ "$children_ok" = 1 ] &&
     [ "$subtree_ok" = 1 ] &&
     grep -q 'Parent body sentinel' "$fixture"; then
    pass folding "TAB, atomic visible motion, folded tails, and generic reveal are safe"
  else
    fail folding "fold state, movement, reveal, or byte preservation failed" "$ORG_SESSION"
    cat "$LEM_YATH_ORG_REPORT"
  fi
fi

# 03: Shift-Tab cycles overview, contents, and all even from body text.
if [ -n "${ORG_SESSION:-}" ]; then
  mx "$ORG_SESSION" lem-yath-test-org-goto-parent-body
  tmux_cmd send-keys -t "$ORG_SESSION" BTab
  sleep 0.3
  overview_ok=0
  if screen_has "$ORG_SESSION" '\* TODO Parent' &&
     screen_has "$ORG_SESSION" '\* Sibling' &&
     screen_lacks "$ORG_SESSION" '\*\* NEXT Child' &&
     screen_lacks "$ORG_SESSION" 'Parent body sentinel'; then
    overview_ok=1
  fi
  tmux_cmd send-keys -t "$ORG_SESSION" BTab
  sleep 0.3
  contents_ok=0
  if screen_has "$ORG_SESSION" '\*\* NEXT Child' &&
     screen_has "$ORG_SESSION" '\*\*\* Grandchild' &&
     screen_lacks "$ORG_SESSION" 'Parent body sentinel' &&
     screen_lacks "$ORG_SESSION" 'Grand body sentinel'; then
    contents_ok=1
  fi
  tmux_cmd send-keys -t "$ORG_SESSION" BTab
  sleep 0.3
  all_ok=0
  screen_has "$ORG_SESSION" 'Parent body sentinel' &&
    screen_has "$ORG_SESSION" 'Grand body sentinel' && all_ok=1
  if [ "$overview_ok" = 1 ] && [ "$contents_ok" = 1 ] &&
     [ "$all_ok" = 1 ]; then
    pass global-fold "Shift-Tab cycles overview/contents/all from body text"
  else
    fail global-fold "global visibility cycle self-cleared or hid the wrong lines" "$ORG_SESSION"
  fi
fi

# 04: heading insertion is structurally safe before siblings and at EOF.
if [ -n "${ORG_SESSION:-}" ]; then
  mx "$ORG_SESSION" lem-yath-test-org-goto-parent
  tmux_cmd send-keys -t "$ORG_SESSION" M-Enter
  sleep 0.2
  tmux_cmd send-keys -t "$ORG_SESSION" -l 'Inserted'
  tmux_cmd send-keys -t "$ORG_SESSION" Escape
  sleep 0.3
  inserted_line=$(lem_capture "$ORG_SESSION" | grep -n '\* Inserted' | head -n1 | cut -d: -f1)
  sibling_heading_line=$(lem_capture "$ORG_SESSION" | grep -n '\* Sibling' | head -n1 | cut -d: -f1)
  before_sibling_ok=0
  [ -n "$inserted_line" ] && [ -n "$sibling_heading_line" ] &&
    [ "$inserted_line" -lt "$sibling_heading_line" ] &&
    screen_lacks "$ORG_SESSION" 'Inserted.*\* Sibling' && before_sibling_ok=1
  mx "$ORG_SESSION" lem-yath-test-org-eof-heading-report
  eof_ok=0
  wait_report '^EOF text="\* Tail\|\* After"$' && eof_ok=1
  if [ "$before_sibling_ok" = 1 ] && [ "$eof_ok" = 1 ]; then
    pass headings "M-Return and EOF insertion create separate complete headings"
  else
    fail headings "heading insertion concatenated text or misplaced point" "$ORG_SESSION"
  fi
fi

# 05: every configured TODO state is saved immediately, including nil.
if [ -n "${ORG_SESSION:-}" ]; then
  mx "$ORG_SESSION" lem-yath-test-org-goto-parent
  todo_ok=1
  for state in NEXT WAITING HOLD SOMEDAY DONE CANCELLED NONE TODO; do
    tmux_cmd send-keys -t "$ORG_SESSION" C-c C-t
    state_ok=0
    for _ in $(seq 1 50); do
      if [ "$state" = NONE ]; then
        grep -q '^\* Parent :work:$' "$fixture" && { state_ok=1; break; }
      else
        grep -q "^\\* $state Parent :work:\$" "$fixture" &&
          { state_ok=1; break; }
      fi
      sleep 0.1
    done
    if [ "$state_ok" != 1 ]; then
      todo_ok=0
      break
    fi
  done
  if [ "$todo_ok" = 1 ] && ! grep -q '^CLOSED:' "$fixture"; then
    pass todo "C-c C-t persists the complete TODO sequence without CLOSED metadata"
  else
    fail todo "the configured TODO sequence diverged on disk at $state" "$ORG_SESSION"
  fi
fi

# 06: reload is idempotent and killing one Org buffer preserves another fold.
if [ -n "${ORG_SESSION:-}" ]; then
  mx "$ORG_SESSION" lem-yath-test-org-goto-parent
  tmux_cmd send-keys -t "$ORG_SESSION" Tab
  mx "$ORG_SESSION" lem-yath-test-org-reload-report
  reload_ok=0
  wait_report '^RELOAD post=1 change=1 kill=1 association=1 folds=0 tab=yes$' &&
    reload_ok=1
  mx "$ORG_SESSION" lem-yath-test-org-kill-cleanup-report
  kill_ok=0
  wait_report '^KILL current=yes survivor-folds=1 victim-live=no$' && kill_ok=1
  if [ "$reload_ok" = 1 ] && [ "$kill_ok" = 1 ]; then
    pass lifecycle "reload deduplicates state and kill cleanup is buffer-local"
  else
    fail lifecycle "reload or multi-buffer kill cleanup diverged" "$ORG_SESSION"
    cat "$LEM_YATH_ORG_REPORT"
  fi
fi

# 07: Org-aware O/o target new checklist items; the stock chord toggles.
write_fixture
if start_org lists; then
  mx "$ORG_SESSION" lem-yath-test-org-goto-list
  tmux_cmd send-keys -t "$ORG_SESSION" O
  sleep 0.2
  tmux_cmd send-keys -t "$ORG_SESSION" -l 'above item'
  tmux_cmd send-keys -t "$ORG_SESSION" Escape
  sleep 0.3
  above_line=$(lem_capture "$ORG_SESSION" | grep -n '\- \[ \] above item' | head -n1 | cut -d: -f1)
  first_line=$(lem_capture "$ORG_SESSION" | grep -n '\- \[ \] first' | head -n1 | cut -d: -f1)
  above_ok=0
  [ -n "$above_line" ] && [ -n "$first_line" ] &&
    [ "$above_line" -lt "$first_line" ] && above_ok=1
  mx "$ORG_SESSION" lem-yath-test-org-goto-list
  tmux_cmd send-keys -t "$ORG_SESSION" o
  sleep 0.2
  tmux_cmd send-keys -t "$ORG_SESSION" -l 'new child'
  tmux_cmd send-keys -t "$ORG_SESSION" Escape
  sleep 0.3
  continuation_ok=0
  screen_has "$ORG_SESSION" '\- \[ \] new child' && continuation_ok=1
  mx "$ORG_SESSION" lem-yath-test-org-goto-list
  tmux_cmd send-keys -t "$ORG_SESSION" C-c C-x C-b
  sleep 0.3
  toggle_ok=0
  screen_has "$ORG_SESSION" '\- \[X\] first' && toggle_ok=1
  if [ "$above_ok" = 1 ] && [ "$continuation_ok" = 1 ] &&
     [ "$toggle_ok" = 1 ]; then
    pass lists "O/o edit the new checklist item and C-c C-x C-b toggles it"
  else
    fail lists "list insertion target, continuation, or checkbox toggle failed" "$ORG_SESSION"
  fi
fi

# 08: table O/TAB target cells; indentation and hline-only tables survive.
if [ -n "${ORG_SESSION:-}" ]; then
  mx "$ORG_SESSION" lem-yath-test-org-goto-table
  tmux_cmd send-keys -t "$ORG_SESSION" O
  sleep 0.2
  tmux_cmd send-keys -t "$ORG_SESSION" -l gamma
  tmux_cmd send-keys -t "$ORG_SESSION" Escape
  sleep 0.3
  gamma_line=$(lem_capture "$ORG_SESSION" | grep -n '| gamma' | head -n1 | cut -d: -f1)
  alpha_line=$(lem_capture "$ORG_SESSION" | grep -n '| alpha' | head -n1 | cut -d: -f1)
  above_row_ok=0
  [ -n "$gamma_line" ] && [ -n "$alpha_line" ] &&
    [ "$gamma_line" -lt "$alpha_line" ] && above_row_ok=1
  mx "$ORG_SESSION" lem-yath-test-org-goto-table
  tmux_cmd send-keys -t "$ORG_SESSION" Tab
  sleep 0.3
  tmux_cmd send-keys -t "$ORG_SESSION" i
  tmux_cmd send-keys -t "$ORG_SESSION" -l X
  tmux_cmd send-keys -t "$ORG_SESSION" Escape
  sleep 0.3
  advance_ok=0
  screen_has "$ORG_SESSION" '\| alpha[[:space:]]+\| Xbeta[[:space:]]+\|' &&
    advance_ok=1
  mx "$ORG_SESSION" lem-yath-test-org-goto-indented-table
  tmux_cmd send-keys -t "$ORG_SESSION" C-c C-c
  sleep 0.3
  mx "$ORG_SESSION" lem-yath-test-org-point-report
  indentation_ok=0
  grep -qF 'text="  | nested' "$LEM_YATH_ORG_REPORT" && indentation_ok=1
  mx "$ORG_SESSION" lem-yath-test-org-goto-hline-only
  tmux_cmd send-keys -t "$ORG_SESSION" C-c C-c
  sleep 0.3
  mx "$ORG_SESSION" lem-yath-test-org-point-report
  hline_ok=0
  grep -qF 'text="|---+-----|"' "$LEM_YATH_ORG_REPORT" && hline_ok=1
  if [ "$above_row_ok" = 1 ] && [ "$advance_ok" = 1 ] &&
     [ "$indentation_ok" = 1 ] && [ "$hline_ok" = 1 ]; then
    pass table "O/TAB target new cells and alignment preserves table structure"
  else
    fail table "row target, cell movement, indentation, or hline handling failed" "$ORG_SESSION"
  fi
fi

# 09: relative file links resolve from the Org buffer's directory.
if [ -n "${ORG_SESSION:-}" ]; then
  mx "$ORG_SESSION" lem-yath-test-org-goto-link
  tmux_cmd send-keys -t "$ORG_SESSION" C-c C-o
  if lem_wait_for "$ORG_SESSION" 'TARGET FILE SENTINEL' 10 >/dev/null; then
    pass links "C-c C-o opened a relative file link"
  else
    fail links "relative link did not open its target" "$ORG_SESSION"
  fi
fi

# 10: promote/demote and reorder operate on the complete heading subtree.
if [ -n "${ORG_SESSION:-}" ]; then
  mx "$ORG_SESSION" lem-yath-test-org-return-fixture
  mx "$ORG_SESSION" lem-yath-test-org-goto-parent
  tmux_cmd send-keys -t "$ORG_SESSION" M-l
  sleep 0.3
  demote_ok=0
  if screen_has "$ORG_SESSION" '\*\* TODO Parent' &&
     screen_has "$ORG_SESSION" '\*\*\* NEXT Child'; then
    demote_ok=1
  fi
  tmux_cmd send-keys -t "$ORG_SESSION" M-h
  sleep 0.3
  restore_ok=0
  screen_has "$ORG_SESSION" '^.*\* TODO Parent' &&
    screen_has "$ORG_SESSION" '\*\* NEXT Child' && restore_ok=1
  tmux_cmd send-keys -t "$ORG_SESSION" M-j
  sleep 0.3
  reorder_ok=0
  sibling_line=$(lem_capture "$ORG_SESSION" | grep -n 'Sibling body sentinel' | head -n1 | cut -d: -f1)
  parent_line=$(lem_capture "$ORG_SESSION" | grep -n 'Parent body sentinel' | head -n1 | cut -d: -f1)
  [ -n "$sibling_line" ] && [ -n "$parent_line" ] &&
    [ "$sibling_line" -lt "$parent_line" ] && reorder_ok=1
  if [ "$demote_ok" = 1 ] && [ "$restore_ok" = 1 ] && [ "$reorder_ok" = 1 ]; then
    pass structure "M-h/l and M-j transform complete subtrees"
  else
    fail structure "subtree promotion, restoration, or reorder failed" "$ORG_SESSION"
  fi
fi

if [ "$FAILED" = 0 ]; then
  echo "ORG TEST PASSED"
  exit 0
fi
echo "ORG TEST FAILED" >&2
exit 1
