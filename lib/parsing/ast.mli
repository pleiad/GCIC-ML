(** This module specifies the structure of the parsed AST *)
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
val to_string : term -> string 