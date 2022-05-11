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
  | Err of term
  | Cast of { source : term; target : term; term : term }

and fun_info = { id : Name.t; dom : term; body : term }
