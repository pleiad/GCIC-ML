(** This module specifies global declarations. 
    It follows Coq's example a bit (https://github.com/coq/coq/blob/master/kernel/declarations.ml)    

    For now we are considering a global declaration to be simply a term and its
    type. 
    We are going for the simplest solution for the moment, but global declarations 
    could have their own type, etc.
*)
open Common.Id

(** Global declaration of constants *)
type const_decl =
  { name : Name.t
  ; ty : Ast.term
  ; term : Ast.term
  }

(** Global declaration of inductives *)
type ind_decl =
  { name : Name.t
  ; params : (Name.t * Ast.term) list
  ; sort : Ast.term
  ; ctors : Name.t list
  }

(** Global declaration of constructor *)
type ctor_decl =
  { name : Name.t
  ; ind : Name.t
  ; params : (Name.t * Ast.term) list
  ; args : (Name.t * Ast.term) list
  }

(** Type of global declaration *)
module type Store = sig
  (** Type of global declaration *)
  type t

  (** Find a global declaration. *)
  val find : Name.t -> t

  (** Add a global declaration. *)
  val add : Name.t -> t -> unit

  (** Checks if the declaration exists. *)
  val exists : Name.t -> bool
end

(** Global declaration of constants *)
module Const : Store with type t = const_decl

(** Global declaration of inductives *)
module Ind : Store with type t = ind_decl

(** Global declaration of constructors *)
module Ctor : Store with type t = ctor_decl
