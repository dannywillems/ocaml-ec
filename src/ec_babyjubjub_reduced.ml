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
      let a = Base.(negate one)

      let d =
        Base.of_string
          "12181644023421730124874158521699555681764249180949974110617291017600649128846"

      let cofactor = Z.of_string "8"

      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [ Base.(
              to_bytes
                (of_string
                   "9671717474070082183213120605117400219616337014328744928644933853176787189663"));
            Base.(
              to_bytes
                (of_string
                   "16950150798460657717958625567821834550301663161624707787222815936182638968203"))
          ]
    end)
