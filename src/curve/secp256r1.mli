module Projective : Ec_sig.ProjectiveWeierstrassT

module Jacobian : Ec_sig.JacobianWeierstrassT

module Affine : Ec_sig.AffineWeierstrassT

val from_affine_to_jacobian : Affine.t -> Jacobian.t

val from_affine_to_projective : Affine.t -> Projective.t

val from_jacobian_to_affine : Jacobian.t -> Affine.t

val from_projective_to_affine : Projective.t -> Affine.t
