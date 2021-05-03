module Iterator : sig
  type bit = int

  module Bit : sig
    type t

    val create_le : Bytes.t -> t

    val create_from_bool_list : bool list -> t

    val next : t -> bit option
  end
end

module MakePedersenHash : functor
  (Ec : Ec_sig.BASE)
  (Params : sig
     val generators : Ec.t list

     val chunks_per_generator : int
   end)
  -> sig
  val hash : Iterator.Bit.t -> Ec.t
end

module Zcash : sig
  val hash : Iterator.Bit.t -> Ec_jubjub.Affine.t
end
