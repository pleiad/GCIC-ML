{
    (* Inside these curly braces we define helper functions that are    
     exposed to our OCaml source code
   *)

  open Parser

  exception SyntaxError of string 
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

(* Regexes for tokens *)
let id = alpha+
let type = "Type"
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let lambda = "lambda"
let product = "Prod"
let unknown = "?"

rule read = parse
| type          { KWD_UNIVERSE }
| lambda        { KWD_LAMBDA }
| product       { KWD_PROD }
| unknown       { KWD_UNKNOWN }
| id            { ID (Lexing.lexeme lexbuf) }
| digit+        { INT (int_of_string (Lexing.lexeme lexbuf)) }
| '{'           { LBRACK }
| '}'           { RBRACK }
| '('           { LPAREN }
| ')'           { RPAREN }
| ':'           { COLON }
| '.'           { DOT }
| '@'           { AT }
| whitespace    { read lexbuf }
| newline       { Lexing.new_line lexbuf ; read lexbuf }
| eof           { EOF }
| _             { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }