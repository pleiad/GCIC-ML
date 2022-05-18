#!/bin/sh

# Based off the example Makefile in Menhir's repo
# https://gitlab.inria.fr/fpottier/menhir/-/blob/master/demos/calc-syntax-errors/Makefile.messages.maintenance

# For simplicity sake, this script is meant to be run at top level
# For example:
# esy update-parser-errors
parsing_lib=lib/parsing

dune exec menhir -- $parsing_lib/parser.mly --list-errors > parserMessages.auto.messages 	
dune exec menhir -- $parsing_lib/parser.mly --merge-errors parserMessages.auto.messages --merge-errors $parsing_lib/parserMessages.messages > parserMessages.merged 	
mv parserMessages.merged $parsing_lib/parserMessages.messages
rm -f parserMessages.auto.messages