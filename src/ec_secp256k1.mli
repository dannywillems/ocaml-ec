(** Implementation of secp256k1, "the bitcoin curve" *)

(** The base field *)
module Fq : Ff_sig.PRIME

(** The scalar field *)
module Fp : Ff_sig.PRIME

module Projective :
  Ec_sig.ProjectiveWeierstrassT
    with type ScalarField.t = Fp.t
     and type BaseField.t = Fq.t
