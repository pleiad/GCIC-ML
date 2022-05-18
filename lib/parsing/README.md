# Overview

We are using [Menhir](http://cambium.inria.fr/~fpottier/menhir/) to generate the parser and [Sedlex](https://github.com/ocaml-community/sedlex) to generate the lexer with Unicode support. Most of the code has comments explaining our decisions (when deemed necessary), but otherwise, the corresponding documentation is the source of truth. 

# Extending/modifying the parser
Whenever you extend or modify the parser, you need to run the script [update_error_msgs.sh](update_error_msgs.sh) in this folder and update the corresponding error messages in [parserMessages.messages](parserMessages.messages). This is because we are using Menhir's [new way of handling errors](http://cambium.inria.fr/~fpottier/menhir/manual.html#sec68), which makes use of the `.messages` file to print better errors, and this new way follows a different maintentance flow.

After making changes to the parser, run the following command:
```bash
# The script is intended to be run inside the parsing folder
cd lib/parsing
. ./update_error_msgs.sh
```
The script should update the `.messages` file with the new possible erroneous states (or updated ones), for which you need to add a proper error message.