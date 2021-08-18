module Curve = struct
  module BabyJubjub = Babyjubjub
  module BabyJubjubReduced = Babyjubjub_reduced
  module BN254 = Bn254
  module BLS12_381 = Bls12_381
  module Curve25519 = Curve25519
  module Curve448 = Curve448
  module Jubjub = Jubjub
  module Pallas = Pallas
  module Secp256k1 = Secp256k1
  module Secp256r1 = Secp256r1
  module Tweedledee = Tweedledee
  module Tweedledum = Tweedledum
  module Vesta = Vesta

  module Utils = struct
    module PBT = Ec_pbt
    module Functor = Ec
  end
end

module CurveSig = Ec_sig

module Hash = struct
  module Poseidon252 = Poseidon252
  module Poseidon128 = Orchard
  module PedersenHash = Pedersen_hash
end

module Signature = struct
  module RedDSA = Reddsa
  module RedJubjub = Redjubjub
end

module Protocol = struct
  module Sapling = struct
    module GroupHash = Group_hash
  end
end

module Digestif = Digestif

module Utils = struct
  module Iterator = Iterator
end
