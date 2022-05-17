(** This module specifies the structure of the parsed AST *)
open Format
open Common.Id

(** Terms in GCIC *)
type term =
  | Var of Name.t
  | Universe of int
  | App of term * term
  | Lambda of (Name.t option * term) list * term
  | Prod of (Name.t option * term) list * term
  | Unknown of int
  | LetIn of (Name.t * term * term * term)

(** Returns the stringified version of a term *)
let rec to_string = function
  | Var x -> Name.to_string x
  | Universe i -> asprintf "Type%i" i
  | App (t, t') -> asprintf "(%s %s)" (to_string t) (to_string t')
  | Lambda (args, b) ->
      asprintf "lambda %s. %s" (string_of_args args) (to_string b)
  | Prod (args, b) ->
      asprintf "forall %s, %s" (string_of_args args) (to_string b)
  | Unknown i -> asprintf "?_%i" i
  | LetIn (id, ty, t1, t2) ->
      asprintf "let %s : %s = %s in %s" (Name.to_string id)
        (to_string ty) (to_string t1) (to_string t2)

and string_of_arg (id, dom) =
  let string_of_name = function
  | None -> "_"
  | Some x -> Name.to_string x in
 asprintf "(%s : %s)" (string_of_name id) (to_string dom)
and string_of_args args = List.map string_of_arg args |> String.concat " "

let rec eq_term t1 t2 =
  match (t1, t2) with
  | (Var x, Var y) -> x = y
  | (Universe i, Universe j) -> i == j
  | (App (t1, u1), App (t2, u2)) -> eq_term t1 t2 && eq_term u1 u2
  | (Lambda (args1,body1), Lambda (args2,body2)) ->
     eq_args args1 args2 && eq_term body1 body2
  | (Prod (args1,body1), Prod (args2,body2)) ->
     eq_args args1 args2 && eq_term body1 body2
  | (Unknown i, Unknown j) -> i == j
  | (LetIn (id1, ty1, t11, t12), LetIn (id2, ty2, t21, t22)) ->
    id1 = id2 && eq_term ty1 ty2 && eq_term t11 t21 && eq_term t12 t22
  | _ -> false
and eq_arg (id1, dom1) (id2, dom2) = id1 = id2 && eq_term dom1 dom2 
and eq_args args1 args2 = List.for_all2 eq_arg args1 args2

type command =
| Eval of term
| Check of term * term
| Elaborate of term

let string_of_command : command -> string = function
  | Eval t -> "Eval " ^ to_string t
  | Check (t, ty) ->
      Format.asprintf "Check %s : %s" (to_string t) (to_string ty)
  | Elaborate t -> "Elaborate " ^ to_string t

let eq_command cmd1 cmd2 =
  match (cmd1, cmd2) with
  | (Eval t1, Eval t2) -> eq_term t1 t2
  | (Check (t1, ty1), Check (t2, ty2)) -> eq_term t1 t2 && eq_term ty1 ty2
  | (Elaborate t1, Elaborate t2) -> eq_term t1 t2
  | _ -> false