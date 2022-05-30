open Common.Id

(* Using a mutable map for now, as the simplest solution. 
We are processing commands sequentially, so the telescope is built naturally, and 
there should be no issue with dependencies between new definitions and previous ones.  *)

type const_decl =
  { name : Name.t
  ; ty : Ast.term
  ; term : Ast.term
  }

type ind_decl =
  { name : Name.t
  ; params : (Name.t * Ast.term) list
  ; sort : Ast.term
  ; ctors : Name.t list
  }

type ctor_decl =
  { name : Name.t
  ; ind : Name.t
  ; params : (Name.t * Ast.term) list
  ; args : (Name.t * Ast.term) list
  }

module type Store = sig
  type t

  val find : Name.t -> t
  val add : Name.t -> t -> unit
  val exists : Name.t -> bool
end

module Make_Store (D : sig
  type t
end) : Store with type t = D.t = struct
  type t = D.t

  let decls : t Name.Map.t ref = ref Name.Map.empty
  let find x = Name.Map.find x !decls
  let add x decl = decls := Name.Map.add x decl !decls
  let exists x = Name.Map.mem x !decls
end

module Const = Make_Store (struct
  type t = const_decl
end)

module Ind = Make_Store (struct
  type t = ind_decl
end)

module Ctor = Make_Store (struct
  type t = ctor_decl
end)
