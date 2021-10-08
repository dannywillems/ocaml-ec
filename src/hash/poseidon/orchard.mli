module Constant : Core.PARAMETERS

module Make : functor (Scalar : Ff_sig.PRIME) -> sig
  module Strategy : Core.STRATEGY with type scalar = Scalar.t

  module Hash : Core.HASH with type scalar = Scalar.t
end

module MakeInplace : functor (Scalar : Core.FF_WITH_INPLACE) -> sig
  module Strategy : Core.STRATEGY with type scalar = Scalar.t

  module Hash : Core.HASH with type scalar = Scalar.t
end
