module Constants : sig
  val width : int

  val nb_constants : int

  val full_rounds : int

  val partial_rounds : int
end

module Make : functor
  (Scalar : sig
     include Ff_sig.PRIME

     val add_inplace : t -> t -> unit

     val mul_inplace : t -> t -> unit

     val copy : t -> t
   end)
  -> sig
  module Strategy : sig
    type state

    val init : Scalar.t array -> state

    val apply_perm : state -> unit

    val get : state -> Scalar.t array
  end

  module Hash : sig
    type ctxt

    val init : unit -> ctxt

    val hash : ctxt -> Scalar.t array -> ctxt

    val get : ctxt -> Scalar.t
  end
end
