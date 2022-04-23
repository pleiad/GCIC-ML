(** This module specifies the structure of the parsed AST *)

(** An abstract type for identifiers*)
module type ID = sig
    type t
  
    val of_string : string -> t
    val to_string : t -> string
    val ( = ) : t -> t -> bool
  end
  
module Var_name : ID 

(** Terms in GCIC *)
type term =
| Var of Var_name.t
| Universe of int 
| App of term * term 
| Lambda of Var_name.t * term * term 
| Prod of Var_name.t * term * term 
| Unknown of int

(** Returns the stringified version of a term *)
val to_string : term -> string 