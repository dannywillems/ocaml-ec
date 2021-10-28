module Affine : sig
  include Ec_sig.AffineEdwardsT

  val of_compressed_exn : Bytes.t -> t

  val of_compressed_opt : Bytes.t -> t option

  val to_compressed : t -> Bytes.t
end

module AffineWeierstrass : Ec_sig.AffineWeierstrassT
