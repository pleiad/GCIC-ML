#!/bin/sh

# Based off the example Makefile in Menhir's repo
# https://gitlab.inria.fr/fpottier/menhir/-/blob/master/demos/calc-syntax-errors/Makefile.messages.maintenance

dune exec menhir -- parser.mly --list-errors > parserMessages.auto.messages 	
dune exec menhir -- parser.mly --merge-errors parserMessages.auto.messages --merge-errors parserMessages.messages > parserMessages.merged 	
mv parserMessages.merged parserMessages.messages
rm -f parserMessages.auto.messages