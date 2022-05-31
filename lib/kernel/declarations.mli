(** This module specifies global declarations. 
    It follows Coq's example a bit (https://github.com/coq/coq/blob/master/kernel/declarations.ml)    

    For now we are considering a global declaration to be simply a term and its
    type. 
    We are going for the simplest solution for the moment, but global declarations 
    could have their own type, etc.
*)
open Common.Id

(** Global declaration of constants *)
type 'a const_decl =
  { name : Name.t
  ; ty : 'a
  ; term : 'a
  }

(** Global declaration of inductives *)
type 'a ind_decl =
  { name : Name.t
  ; params : (Name.t * 'a) list
  ; sort : 'a
  ; ctors : Name.t list
  }

(** Global declaration of constructor *)
type 'a ctor_decl =
  { name : Name.t
  ; ind : Name.t
  ; params : (Name.t * 'a) list
  ; args : (Name.t * 'a) list
  ; ty : 'a
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
module Const : Store with type t = Ast.term const_decl

(** Global declaration of inductives *)
module Ind : Store with type t = Ast.term ind_decl

(** Global declaration of constructors *)
module Ctor : Store with type t = Ast.term ctor_decl
