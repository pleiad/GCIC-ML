open Parser

exception SyntaxError of string

let lexeme = Sedlexing.Utf8.lexeme
let rollback = Sedlexing.rollback

(* Define helper regexes *)
let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let lower = [%sedlex.regexp? 'a' .. 'z']
let upper = [%sedlex.regexp? 'A' .. 'Z']
let alpha = [%sedlex.regexp? lower | upper]
let id = [%sedlex.regexp? lower, Star (alpha | digit | '_')]
let universe = [%sedlex.regexp? "Type" | 0x25a1]
let whitespace = [%sedlex.regexp? Plus ('\t' | ' ')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let lambda = [%sedlex.regexp? "fun" | 0x03bb]
let forall = [%sedlex.regexp? "forall" | 0x2200]
let unknown = [%sedlex.regexp? '?']
let arrow = [%sedlex.regexp? "->" | 0x2192]

let rec token lexbuf =
  match%sedlex lexbuf with
  | universe -> KWD_UNIVERSE
  | lambda -> KWD_LAMBDA
  | forall -> KWD_FORALL
  | unknown -> KWD_UNKNOWN
  | "let" -> KWD_LET
  | "in" -> KWD_IN
  | "check" -> VERNAC_CHECK
  | "eval" -> VERNAC_EVAL
  | "elab" -> VERNAC_ELABORATE
  | ";;" -> VERNAC_SEPARATOR
  | id -> ID (lexeme lexbuf)
  | number -> INT (int_of_string (lexeme lexbuf))
  | arrow -> ARROW
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '=' -> EQUAL
  | ':' -> COLON
  | '.' -> DOT
  | ',' -> COMMA
  | whitespace -> token lexbuf
  | newline ->
    Sedlexing.new_line lexbuf;
    token lexbuf
  | eof -> EOF
  | any -> raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf))
  | _ -> assert false
