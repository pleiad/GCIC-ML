(** This module specifies global declarations. 
    It follows Coq's example a bit (https://github.com/coq/coq/blob/master/kernel/declarations.ml)    

    For now we are considering a global declaration to be simply a term and its
    type. 
    We are going for the simplest solution for the moment, but global declarations 
    could have their own type, etc.
*)
open Common.Id

(** Find a global declaration.
    Raises [Not_found] if no binding for the name exists. *)
val find : Name.t -> Ast.term * Ast.term

(** Add a global declaration. *)
val add : Name.t -> Ast.term * Ast.term -> unit

(** Checks if the declaration exists. *)
val exists : Name.t -> bool
