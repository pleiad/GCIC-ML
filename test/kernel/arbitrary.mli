open Kernel

(** Generator for arbitrary types in kernel *)

(** Arbitrary cast_cic term*)
val arbitrary_cast_cic_term : Cast_cic.term QCheck.arbitrary