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
let filename = [%sedlex.regexp? alpha, Star (alpha | digit | '_')]
let universe = [%sedlex.regexp? "Type" | 0x25a1]
let whitespace = [%sedlex.regexp? Plus ('\t' | ' ')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let lambda = [%sedlex.regexp? "fun" | 0x03bb]
let forall = [%sedlex.regexp? "forall" | 0x2200]
let unknown = [%sedlex.regexp? '?']
let unknownT = [%sedlex.regexp? "?T"]
let arrow = [%sedlex.regexp? "->" | 0x2192]
let stringbuf = Buffer.create 64

let get_and_flush buffer =
  let s = Buffer.contents buffer in
  Buffer.clear buffer;
  s

let rec token lexbuf =
  match%sedlex lexbuf with
  | universe -> KWD_UNIVERSE
  | lambda -> KWD_LAMBDA
  | forall -> KWD_FORALL
  | unknown -> KWD_UNKNOWN
  | unknownT -> KWD_UNKNOWN_T
  | "let" -> KWD_LET
  | "in" -> KWD_IN
  | "check" -> VERNAC_CHECK
  | "eval" -> VERNAC_EVAL
  | "elab" -> VERNAC_ELABORATE
  | "def" -> VERNAC_DEFINITION
  | "set" -> VERNAC_SET
  | "variant" -> VERNAC_FLAG_VARIANT
  | "fuel" -> VERNAC_FLAG_FUEL
  | "load" -> VERNAC_LOAD
  | ";;" -> VERNAC_SEPARATOR
  | "G" -> VERNAC_VARIANT_G
  | "N" -> VERNAC_VARIANT_N
  | "S" -> VERNAC_VARIANT_S
  | id -> ID (lexeme lexbuf)
  | number -> INT (int_of_string (lexeme lexbuf))
  | arrow -> ARROW
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '=' -> EQUAL
  | ':' -> COLON
  | '.' -> DOT
  | ',' -> COMMA
  | '"' -> FILENAME (string lexbuf stringbuf)
  | whitespace -> token lexbuf
  | newline ->
    Sedlexing.new_line lexbuf;
    token lexbuf
  | eof -> EOF
  | any -> raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf))
  | _ -> assert false

and string lexbuf stringbuf =
  match%sedlex lexbuf with
  | '"' | eof -> get_and_flush stringbuf
  | newline ->
    Sedlexing.new_line lexbuf;
    string lexbuf stringbuf
  | any ->
    Buffer.add_string stringbuf (lexeme lexbuf);
    string lexbuf stringbuf
  | _ -> assert false
