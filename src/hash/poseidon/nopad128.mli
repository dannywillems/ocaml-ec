module Constant : Core.PARAMETERS

module Make (Scalar : Ff_sig.PRIME) : sig
  module Strategy : Core.STRATEGY with type scalar = Scalar.t

  module Hash : Core.HASH with type scalar = Scalar.t
end
