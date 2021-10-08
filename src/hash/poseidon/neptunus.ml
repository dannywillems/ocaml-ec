module Constant : Core.PARAMETERS = struct
  let width = 3

  let full_rounds = 60

  let partial_rounds = 0

  (* FIXME: MUST BE CHANGED. Not sure it is secure *)
  let mds_matrix = Mds_orchard.v

  let round_constants = Ark_neptunus.v

  let partial_round_idx_to_permute = 4
end

module Make (Scalar : Ff_sig.PRIME) = Core.Make (Constant) (Scalar)
