include Common.Declarations

module Const = Make_Store (struct
  type t = Kernel.Ast.term const_decl
  type cached_t = Ast.term const_decl
end)

module Ind = Make_Store (struct
  type t = Kernel.Ast.term ind_decl
  type cached_t = Ast.term ind_decl
end)

module Ctor = Make_Store (struct
  type t = Kernel.Ast.term ctor_decl
  type cached_t = Ast.term ctor_decl
end)
