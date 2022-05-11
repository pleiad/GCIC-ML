(** This module specifies the elaboration from GCIC to CastCIC *)

type elaboration_error 
(** An error originating from elaboration  *)

type elaboration = Ast.term * Ast.term 
(** The elaboration result  *)

val elaborate : Ast.context -> Gcic.Ast.term -> (elaboration, elaboration_error) result
(** The elaboration procedure, as per the paper *)

