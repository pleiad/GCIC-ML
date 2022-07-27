open Common.Id

module type Typing = sig
  type t
  type i
  type c

  val check_type : t Name.Map.t -> t -> t -> c
  val infer_type : t Name.Map.t -> t -> i
end
