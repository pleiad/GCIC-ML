(** This module specifies the context for CastCIC (ie. Gamma) *)

module NameMap = Map.Make (Id.Name)

type 'a t = 'a NameMap.t

let empty = NameMap.empty
let add = NameMap.add
let find = NameMap.find

let lookup id ctx =
  try Some (find id ctx) with
  | Not_found -> None

let map = NameMap.map

(*
let rec string_of_seq_context context : string =
  match context with
  | [] -> "[]"
  | (k, (None, ty)) :: tl ->
    Format.asprintf
      "(%s, axiom %s) ; %s"
      (Name.to_string k)
      (Ast.to_string ty)
      (string_of_seq_context tl)
  | (k, (Some t, ty)) :: tl ->
    Format.asprintf
      "(%s, %s : %s) ; %s"
      (Name.to_string k)
      (Ast.to_string t)
      (Ast.to_string ty)
      (string_of_seq_context tl)

let to_string ctx : string = NameMap.bindings ctx |> string_of_seq_context
*)
