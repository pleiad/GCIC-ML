(** This module specifies the structure of the parsed AST *)
open Format
open Common.Id


(** Terms in GCIC *)
type term =
  | Var of Name.t
  | Universe of int
  | App of term * term
  | Lambda of Name.t * term * term
  | Prod of Name.t * term * term
  | Unknown of int

(** Returns the stringified version of a term *)
let rec to_string = function
  | Var x -> Name.to_string x
  | Universe i -> asprintf "Universe_%i" i
  | App (t, t') -> asprintf "(%s %s)" (to_string t) (to_string t')
  | Lambda (x, t, b) ->
      asprintf "lambda %s : %s. %s" (Name.to_string x) (to_string t)
        (to_string b)
  | Prod (x, a, b) ->
      asprintf "Prod %s : %s. %s" (Name.to_string x) (to_string a)
        (to_string b)
  | Unknown i -> asprintf "?_%i" i
