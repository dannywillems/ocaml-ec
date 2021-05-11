(* https://datatracker.ietf.org/doc/html/rfc7748#section-4.2 *)
module Base = Ff.MakeFp (struct
  let prime_order = Z.(pow (of_int 2) 448 - pow (of_int 2) 224 - one)
end)

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.(
      pow (of_int 2) 446
      - of_string "0x8335dc163bb124b65129c96fde933d8d723a70aadc873d6d54a7bb0d")
end)

module Affine =
  Ec.MakeAffineEdwards (Base) (Scalar)
    (struct
      let a = Base.of_string "1"

      let d =
        Base.of_string
          "611975850744529176160423220965553317543219696871016626328968936415087860042636474891785599283666020414768678979989378147065462815545017"

      let cofactor = Z.of_string "4"

      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [ Base.(
              to_bytes
                (of_string
                   "345397493039729516374008604150537410266655260075183290216406970281645695073672344430481787759340633221708391583424041788924124567700732"));
            Base.(
              to_bytes
                (of_string
                   "363419362147803445274661903944002267176820680343659030140745099590306164083365386343198191849338272965044442230921818680526749009182718"))
          ]
    end)
