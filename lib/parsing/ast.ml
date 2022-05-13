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

let rec eq_term t1 t2 =
  match (t1, t2) with
  | (Var x, Var y) -> x = y
  | (Universe i, Universe j) -> i == j
  | (App (t1, u1), App (t2, u2)) -> eq_term t1 t2 && eq_term u1 u2
  | (Lambda (id1,dom1,body1), Lambda (id2,dom2,body2)) ->
     id1 = id2 && eq_term dom1 dom2 && eq_term body1 body2
  | (Prod (id1,dom1,body1), Prod (id2,dom2,body2)) ->
     id1 = id2 && eq_term dom1 dom2 && eq_term body1 body2
  | (Unknown i, Unknown j) -> i == j
  | _ -> false