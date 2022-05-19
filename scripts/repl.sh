#!/bin/bash

if command -v rlwrap > /dev/null; then
RLWRAP="rlwrap -n -f scripts/repl.completion"
fi
$RLWRAP dune exec gcic 2> errors.log