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

  type cached_t

  (** Find a global declaration. *)
  val find : Name.t -> cached_t

  (** Add a global declaration. *)
  val add : Name.t -> t -> unit

  (** Checks if the declaration exists. *)
  val exists : Name.t -> bool

  (** Cache the declaratoin using the current variant *)
  val add_cache : Name.t -> cached_t -> unit
end

module Make_Store (D : sig
  type t
  type cached_t
end) : Store with type t = D.t with type cached_t = D.cached_t
