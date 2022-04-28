(** This module specifies the typing relation *)

open Cast_cic


let reduce1 (t : term) : term =
  t

let reduce (t : term) : term =
  t

type type_error = string

let infering (ctx : context) (t : term) : (term, type_error) result =
  Error ""

and checking (ctx : context) (t : term) (ty : term) : (unit, type_error) result =
  Error ""

and infering_prod (ctx : context) (t : term) : (Name.t * term * term, type_error) result =
  Error ""

and infering_univ (ctx : context) (t : term) : (int, type_error) result =
  Error ""