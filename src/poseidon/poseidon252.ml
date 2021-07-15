module Constant : Core.PARAMETERS = struct
  let width = 5

  let nb_constants = 960

  let full_rounds = 8

  let partial_rounds = 59

  let mds_matrix = Mds_poseidon252.v

  let round_constants = Ark_poseidon252.v
end

module Make (Scalar : Ff_sig.PRIME) = Core.Make (Constant) (Scalar)
