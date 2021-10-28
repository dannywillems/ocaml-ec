module Affine : sig
  include Ec_sig.AffineEdwardsT

  val of_compressed_exn : Bytes.t -> t

  val of_compressed_opt : Bytes.t -> t option

  val to_compressed : t -> Bytes.t
end

module AffineEdwards : Ec_sig.AffineEdwardsT

module AffineWeierstrass : Ec_sig.AffineWeierstrassT

module AffineMontgomery : Ec_sig.AffineMontgomeryT

val from_twisted_to_montgomery : AffineEdwards.t -> AffineMontgomery.t option

val from_montgomery_to_twisted : AffineMontgomery.t -> AffineEdwards.t option

val from_montgomery_to_weierstrass :
  AffineMontgomery.t -> AffineWeierstrass.t option

val from_twisted_to_weierstrass : AffineEdwards.t -> AffineWeierstrass.t option
