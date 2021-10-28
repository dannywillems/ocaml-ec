module Affine : Ec_sig.AffineEdwardsT

module AffineMontgomery : Ec_sig.AffineMontgomeryT

val from_twisted_to_montgomery : Affine.t -> AffineMontgomery.t option

val from_montgomery_to_twisted : AffineMontgomery.t -> Affine.t option
