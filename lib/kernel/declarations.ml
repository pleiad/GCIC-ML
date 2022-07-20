open Common.Id

type 'a const_decl =
  { name : Name.t
  ; ty : 'a
  ; term : 'a
  }

type 'a ind_decl =
  { name : Name.t
  ; params : (Name.t * 'a) list
  ; sort : 'a
  ; level : int
  ; ctors : Name.t list
  }

type 'a ctor_decl =
  { name : Name.t
  ; ind : Name.t
  ; params : (Name.t * 'a) list
  ; args : (Name.t * 'a) list
  ; ty : 'a
  }

module type Store = sig
  type t
  type cached_t

  val find : Name.t -> cached_t
  val add : Name.t -> t -> unit
  val exists : Name.t -> bool
  val add_cache : Name.t -> cached_t -> unit
end

(* Storing the data in mutable maps *)
module Make_Store (D : sig
  type t
  type cached_t
end) : Store with type t = D.t with type cached_t = D.cached_t = struct
  type t = D.t
  type cached_t = D.cached_t

  let decls : t Name.Map.t ref = ref Name.Map.empty
  let cached_decls : cached_t Name.Map.t ref = ref Name.Map.empty
  let find x = Name.Map.find x !cached_decls
  let add x decl = decls := Name.Map.add x decl !decls
  let exists x = Name.Map.mem x !decls
  let add_cache x decl = cached_decls := Name.Map.add x decl !cached_decls
end
