#!/bin/sh

cd lib/parsing
dune exec menhir -- parser.mly --list-errors > parserMessages.auto.messages 	
dune exec menhir -- parser.mly --merge-errors parserMessages.auto.messages --merge-errors parserMessages.messages > parserMessages.merged 	
mv parserMessages.merged parserMessages.messages
rm -f parserMessages.auto.messages