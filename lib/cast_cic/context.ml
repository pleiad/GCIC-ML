(** This module specifies the context for CastCIC (ie. Gamma) *)

open Common.Id
module NameMap = Map.Make (Name)

type context = Ast.term NameMap.t

let rec string_of_seq_context context : string =
  match context with
  | [] -> "[]"
  | (k, v) :: tl ->
    Format.asprintf
      "(%s, %s) ; %s"
      (Name.to_string k)
      (Ast.to_string v)
      (string_of_seq_context tl)

let string_of_context ctx : string = NameMap.bindings ctx |> string_of_seq_context
