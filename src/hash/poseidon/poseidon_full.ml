module Constant : Core.PARAMETERS = struct
  let width = 3

  let full_rounds = 60

  let partial_rounds = 0

  (* MUST BE CHANGED. Not sure it is secure *)
  let mds_matrix = Mds_orchard.v

  let round_constants = Ark_poseidon_full_round.v

  let partial_round_idx_to_permute = 4
end

module Make (Scalar : Ff_sig.PRIME) = Core.Make (Constant) (Scalar)
