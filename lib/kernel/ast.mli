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
  (* Extras *)
  | Ascription of term * term
  | UnknownT of int
  | Const of Name.t

and fun_info =
  { id : Name.t
  ; dom : term
  ; body : term
  }

(** Returns the stringified version of a term *)
val to_string : term -> string
