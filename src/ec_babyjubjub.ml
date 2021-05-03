(* https://eips.ethereum.org/EIPS/eip-2494#specification *)
(* Check it is not a small order element *)

module Base = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "21888242871839275222246405745257275088548364400416034343698204186575808495617"
end)

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "2736030358979909402780800718157159386076813972158567259200215660948447373041"
end)

module Affine =
  Ec.MakeAffineEdwards (Base) (Scalar)
    (struct
      let a = Base.of_string "168700"

      let d = Base.of_string "168696"

      let cofactor = Z.of_string "8"

      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [ Base.(
              to_bytes
                (of_string
                   "5299619240641551281634865583518297030282874472190772894086521144482721001553"));
            Base.(
              to_bytes
                (of_string
                   "16950150798460657717958625567821834550301663161624707787222815936182638968203"))
          ]
    end)