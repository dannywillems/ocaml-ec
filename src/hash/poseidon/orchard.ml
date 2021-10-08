module Constant : Core.PARAMETERS = struct
  let width = 3

  let full_rounds = 8

  let partial_rounds = 58

  let mds_matrix = Mds_orchard.v

  let round_constants = Ark_orchard.v

  let partial_round_idx_to_permute = 0
end

module Make (Scalar : Ff_sig.PRIME) = Core.Make (Constant) (Scalar)
module MakeInplace (Scalar : Core.FF_WITH_INPLACE) =
  Core.MakeInplace (Constant) (Scalar)
