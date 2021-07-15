module type PARAMETERS = sig
  val width : int

  val full_rounds : int

  val partial_rounds : int

  val round_constants : string array

  val mds_matrix : string array array
end

module type STRATEGY = sig
  type scalar

  type state

  val init : scalar array -> state

  val apply_perm : state -> unit

  val get : state -> scalar array
end

module type HASH = sig
  type scalar

  type ctxt

  val init : unit -> ctxt

  val hash : ctxt -> scalar array -> ctxt

  val get : ctxt -> scalar
end

module Make : functor (C : PARAMETERS) (Scalar : Ff_sig.PRIME) -> sig
  module Strategy : STRATEGY with type scalar = Scalar.t

  module Hash : HASH with type scalar = Scalar.t
end
