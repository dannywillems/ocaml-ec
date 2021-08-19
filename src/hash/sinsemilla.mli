open Pallas

exception Bottom

module MakeSinsemilla : functor
  (Params : sig
     val init_value : Affine.t

     val generators : Affine.t array

     val chunk_size : int
   end)
  -> sig
  (** Can raise [Bottom] *)
  val hash_exn : Iterator.Bit.t -> Affine.Base.t

  val hash_opt : Iterator.Bit.t -> Affine.Base.t option
end
