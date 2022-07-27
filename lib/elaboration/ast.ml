(** This module specifies the structure of CastCIC *)
open Common.Id

(** Global counter used to create new identifiers *)
let id_counter : int ref = ref 0

(** Returns a new identifier name *)
let new_identifier () : Name.t =
  let id = Name.of_string ("#" ^ string_of_int !id_counter) in
  id_counter := !id_counter + 1;
  id

(** Terms in CastCIC *)
type elaborated_term =
  | Var of Name.t
  | Universe of int
  | App of elaborated_term * elaborated_term
  | Lambda of fun_info
  | Prod of fun_info
  | Unknown of elaborated_term
  | Err of elaborated_term
  | Cast of
      { source : elaborated_term
      ; target : elaborated_term
      ; term : elaborated_term
      }
  | Const of Name.t
  (* Inductives *)
  | Inductive of Name.t * int * elaborated_term list
  | Constructor of
      { ctor : Name.t
      ; level : int
      ; params : elaborated_term list
      ; args : elaborated_term list
      }
  | Match of
      { ind : Name.t
      ; discr : elaborated_term
      ; z : Name.t
      ; pred : elaborated_term
      ; f : Name.t
      ; branches : branch list
      }

and fun_info =
  { id : Name.t
  ; dom : elaborated_term
  ; body : elaborated_term
  }

and branch =
  { ctor : Name.t
  ; ids : Name.t list
  ; term : elaborated_term
  }

(** Pretty printer *)
module Pretty = struct
  open Fmt

  (** Returns if a term requires a parenthesis for unambiguation *)
  let need_parens = function
    | Lambda _ | Prod _ | Cast _ | Match _ -> true
    | Inductive (_, _, args) -> args <> []
    | Constructor { params; args; _ } -> params <> [] || args <> []
    | _ -> false

  let rec group_lambda_args acc = function
    | Lambda { id; dom; body } -> group_lambda_args ((id, dom) :: acc) body
    | t -> List.rev acc, t

  let rec group_prod_args acc = function
    | Prod { id; dom; body } when not (Name.is_default id) ->
      group_prod_args ((id, dom) :: acc) body
    | t -> List.rev acc, t

  (** Pretty printer for term *)
  let rec pp ppf = function
    | Var x -> pf ppf "%a" Name.pp x
    | Universe 0 -> pf ppf "▢"
    | Universe i -> pf ppf "▢%i" i
    | App (t, t') -> pf ppf "@[%a@ %a@]" maybe_parens t maybe_parens t'
    | Lambda _ as t -> group_lambda_args [] t |> pp_lambda ppf
    | Prod { id; dom; body } as t ->
      if Name.is_default id
      then pf ppf "@[<hov 1>%a →@ %a@]" pp dom pp body
      else group_prod_args [] t |> pp_prod ppf
    | Unknown ty -> pf ppf "?_%a" pp ty
    | Err ty -> pf ppf "err_%a" pp ty
    | Cast { source; target; term } ->
      pf ppf "@[<hov 1>⟨%a ⇐@ %a⟩@ %a@]" pp target pp source pp term
    | Const x -> pf ppf "%a" Name.pp x
    | Inductive (ind, _, []) -> pf ppf "%a" Name.pp ind
    | Inductive (ind, _, params) ->
      pf ppf "@[%a@ %a@]" Name.pp ind (list ~sep:sp maybe_parens) params
    | Constructor { ctor; params = []; args = []; _ } -> pf ppf "%a" Name.pp ctor
    | Constructor { ctor; params; args; _ } ->
      pf ppf "@[%a@ %a@]" Name.pp ctor (list ~sep:sp maybe_parens) (params @ args)
    | Match { discr; z; pred; _ } ->
      pf ppf "@[match %a as %a return@ %a with@]" pp discr Name.pp z pp pred

  (** Pretty-prints an argument of a lambda or prod *)
  and pp_arg ppf (x, ty) = pf ppf "@[(%a : %a)@]" Name.pp x pp ty

  (** Pretty-prints a lambda *)
  and pp_lambda ppf (args, body) =
    pf ppf "@[<hov 1>λ%a ⇒@ %a@]" (list ~sep:sp pp_arg) args pp body

  (** Pretty-prints a prod *)
  and pp_prod ppf (args, body) =
    pf ppf "@[<hov 1>∀%a,@ %a@]" (list ~sep:sp pp_arg) args pp body

  (** Adds parenthesis around a term if needed *)
  and maybe_parens ppf t = if need_parens t then parens pp ppf t else pp ppf t

  (** Returns the prettified version of a term *)
  let to_string = to_to_string pp

  (** Prints the prettified version of a term *)
  let print = pp Format.std_formatter
end

(** Pretty printer for term *)
let pp_term = Pretty.pp

(** Returns the prettified version of a term *)
let to_string = Pretty.to_string

(** Prints the prettified version of a term *)
let print = Pretty.print

(* Module alias *)
module Context = Name.Map

(** Performs substitution inside a term *)
let rec subst ctx = function
  | Var x ->
    (try Context.find x ctx with
    | Not_found -> Var x)
  | Universe i -> Universe i
  | App (t, u) -> App (subst ctx t, subst ctx u)
  | Lambda fi ->
    Lambda
      { fi with
        dom = subst ctx fi.dom
      ; body = subst (Context.add fi.id (Var fi.id) ctx) fi.body
      }
  | Prod fi ->
    Prod
      { fi with
        dom = subst ctx fi.dom
      ; body = subst (Context.add fi.id (Var fi.id) ctx) fi.body
      }
  | Unknown t -> Unknown (subst ctx t)
  | Err t -> Err (subst ctx t)
  | Cast { source; target; term } ->
    Cast { source = subst ctx source; target = subst ctx target; term = subst ctx term }
  | Const x -> Const x
  | Inductive (ind, i, params) -> Inductive (ind, i, List.map (subst ctx) params)
  | Constructor ci ->
    Constructor
      { ci with
        params = List.map (subst ctx) ci.params
      ; args = List.map (subst ctx) ci.args
      }
  | Match mi ->
    Match
      { mi with
        discr = subst ctx mi.discr
      ; pred = subst (Context.add mi.z (Var mi.z) ctx) mi.pred
      ; branches = List.map (subst_branch ctx) mi.branches
      }

and subst_branch ctx br =
  let ids_ctx = List.map (fun x -> x, Var x) br.ids |> List.to_seq in
  let ext_ctx = Context.add_seq ids_ctx ctx in
  { br with term = subst ext_ctx br.term }

let subst1 x v = subst (Context.add x v Context.empty)

(** Checks if two terms are identifiable up to alpha-renaming *)
let rec alpha_equal t1 t2 =
  match t1, t2 with
  | Var x, Var y -> x = y
  | Universe i, Universe j -> i = j
  | App (t1, u1), App (t2, u2) -> alpha_equal t1 t2 && alpha_equal u1 u2
  | Lambda fi1, Lambda fi2 ->
    let x_id = new_identifier () in
    let x = Var x_id in
    let body1 = subst1 fi1.id x fi1.body in
    let body2 = subst1 fi2.id x fi2.body in
    alpha_equal fi1.dom fi2.dom && alpha_equal body1 body2
  | Prod fi1, Prod fi2 ->
    let x_id = new_identifier () in
    let x = Var x_id in
    let body1 = subst1 fi1.id x fi1.body in
    let body2 = subst1 fi2.id x fi2.body in
    alpha_equal fi1.dom fi2.dom && alpha_equal body1 body2
  | Unknown t1, Unknown t2 -> alpha_equal t1 t2
  | Err t1, Err t2 -> alpha_equal t1 t2
  | Cast ci1, Cast ci2 ->
    alpha_equal ci1.source ci2.source
    && alpha_equal ci1.target ci2.target
    && alpha_equal ci1.term ci2.term
  | Const x, Const y -> x = y
  | Inductive (ind1, i1, params1), Inductive (ind2, i2, params2) ->
    ind1 = ind2 && i1 = i2 && List.equal alpha_equal params1 params2
  | Constructor c1, Constructor c2 ->
    c1.ctor = c2.ctor
    && c1.level = c2.level
    && List.equal alpha_equal c1.args c2.args
    && List.equal alpha_equal c1.params c2.params
  | Match m1, Match m2 ->
    let alpha_equal_branch b1 b2 =
      if List.compare_lengths b1.ids b2.ids = 0
      then (
        let new_ids = List.map (fun _ -> Var (new_identifier ())) b1.ids in
        let ids1 = List.combine b1.ids new_ids in
        let ids2 = List.combine b1.ids new_ids in
        let subst_body body ids =
          List.fold_left (fun body (old_id, new_id) -> subst1 old_id new_id body) body ids
        in
        let subst_body1 = subst_body b1.term ids1 in
        let subst_body2 = subst_body b2.term ids2 in
        b1.ctor = b2.ctor && alpha_equal subst_body1 subst_body2)
      else false
    in
    m1.ind = m2.ind
    && alpha_equal m1.discr m2.discr
    && m1.z = m2.z
    && alpha_equal m1.pred m2.pred
    && List.equal alpha_equal_branch m1.branches m2.branches
  | _ -> false

let rec subst_tele ?(acc = []) ts params =
  match ts, params with
  | [], [] -> List.rev acc
  | t :: ts, (x, ty) :: params ->
    let params' = List.map (fun (y, ty) -> y, subst1 x t ty) params in
    subst_tele ~acc:(ty :: acc) ts params'
  (* Fails on partial application of inductives or constructors *)
  | _ -> failwith "telescopic substitution over lists of different size"

let subst1_tele t params =
  match params with
  | [] -> []
  | (x, _) :: params -> List.map (fun (y, ty) -> y, subst1 x t ty) params
