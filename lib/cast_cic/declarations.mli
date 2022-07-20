open Common.Declarations

(** Global declaration of constants *)
module Const :
  Store with type t = Kernel.Ast.term const_decl with type cached_t = Ast.term const_decl

(** Global declaration of inductives *)
module Ind :
  Store with type t = Kernel.Ast.term ind_decl with type cached_t = Ast.term ind_decl

(** Global declaration of constructors *)
module Ctor :
  Store with type t = Kernel.Ast.term ctor_decl with type cached_t = Ast.term ctor_decl
