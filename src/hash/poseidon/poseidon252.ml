module Constant : Core.PARAMETERS = struct
  let width = 5

  let full_rounds = 8

  let partial_rounds = 59

  let mds_matrix = Mds_poseidon252.v

  let round_constants = Ark_poseidon252.v

  let partial_round_idx_to_permute = 4

  let with_padding = true
end

module Make (Scalar : Ff_sig.PRIME) = Core.Make (Constant) (Scalar)
