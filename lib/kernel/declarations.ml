open Common.Id

(* Using a mutable map for now, as the simplest solution. 
We are processing commands sequentially, so the telescope is built naturally, and 
there should be no issue with dependencies between new definitions and previous ones.  *)

type 'a const_decl =
  { name : Name.t
  ; ty : 'a
  ; term : 'a
  }

type 'a ind_decl =
  { name : Name.t
  ; params : (Name.t * 'a) list
  ; sort : 'a
  ; ctors : Name.t list
  }

type 'a ctor_decl =
  { name : Name.t
  ; ind : Name.t
  ; params : (Name.t * 'a) list
  ; args : (Name.t * 'a) list
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
  type t = Ast.term const_decl
end)

module Ind = Make_Store (struct
  type t = Ast.term ind_decl
end)

module Ctor = Make_Store (struct
  type t = Ast.term ctor_decl
end)
