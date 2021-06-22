module Fq : Ff_sig.PRIME

module Fp : Ff_sig.PRIME

module Projective :
  Ec_sig.ProjectiveWeierstrassT
    with type BaseField.t = Fq.t
     and type ScalarField.t = Fp.t

module Iso : sig
  module Projective :
    Ec_sig.ProjectiveWeierstrassT
      with type BaseField.t = Fq.t
       and type ScalarField.t = Fp.t
end

val iso_map : Iso.Projective.t -> Projective.t
