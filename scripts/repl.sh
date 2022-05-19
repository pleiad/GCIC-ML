#!/bin/bash

# This is a quick fix to get a better REPL experience
# via rlwrap. If rlwrap is not installed in the system
# the basic REPL will be executed.
# Note that we use the repl.completion file to provide
# auto-completion.
if command -v rlwrap > /dev/null; then
RLWRAP="rlwrap -n -f scripts/repl.completion"
fi
$RLWRAP dune exec gcic 2> errors.log