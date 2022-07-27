module type Elaboration = sig
  type o

  val elaborate : GCIC.term -> o
end