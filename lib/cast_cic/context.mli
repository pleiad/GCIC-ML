(** This module specifies the context for CastCIC (ie. Gamma) *)

(* FIXME: This should probably go in another place, it's not contained to CastCIC
   (maybe in Common?). Since it's so small and only CastCIC uses it, it'll live here
   for now *)

(** The mapping from identifiers to some type *)
module NameMap : Map.S with type key = Common.Id.Name.t

(** The specific mapping from identifiers to terms *)
type context = Ast.term NameMap.t

(** Returns the stringified version of the context *)
val string_of_context : context -> string
