(** This module specifies the structure of GCIC *)
open Common.Id

(** Terms in GCIC *)
type term =
  | Var of Name.t
  | Universe of int
  | App of term * term
  | Lambda of fun_info
  | Prod of fun_info
  | Unknown of int

and fun_info = { id : Name.t; dom : term; body : term }

val to_string : term -> string
(** Returns the stringified version of a term *)