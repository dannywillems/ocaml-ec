(**
  https://eprint.iacr.org/2021/1152.pdf

  Base field: 52435875175126190479447740508185965837690552500527637822603658699938581184513 (254 bits - 32 bytes)
  Scalar field: 13108968793781547619861935127046491459309155893440570251786403306729687672801 (253 bits - 32 bytes)

  Base field multiplicative subgroup decomposition:
    2^32 * 3 * 11 * 19 * 10177 * 125527 * 859267 * 906349^2 * 2508409 * 2529403 * 52437899 * 254760293^2
  Prime field multiplication subgroup decomposition:
    2^5 * 3 * 5^2 * 5462070330742311508275806302936038108045481622266904271577668044470703197
*)

module Base = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "13108968793781547619861935127046491459309155893440570251786403306729687672801"
end)

(* module AffineEdwards =
  Ec.MakeAffineEdwards (Base) (Scalar)
    (struct
      let a = Base.(negate (of_string "5"))

      let d =
        Base.(
          of_string "138827208126141220649022263972958607803"
          / of_string "171449701953573178309673572579671231137")

      let cofactor = Z.of_string "4"

      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [ Base.(
              to_bytes
                (of_string
                   "18886178867200960497001835917649091219057080094937609519140440539760939937304"));
            Base.(
              to_bytes
                (of_string
                   "19188667384257783945677642223292697773471335439753913231509108946878080696678"))
          ]
    end) *)

(* module AffineMontgomery =
  Ec.MakeAffineMontgomery (Base) (Scalar)
    (struct
      let a =
        Base.of_string
          "29978822694968839326280996386011761570173833766074948509196803838190355340952"

      let b =
        Base.of_string
          "21732574493545642452588025716306585039145419364997213261322953924237652797223"

      let cofactor = Z.of_int 4

      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [ Base.(
              to_bytes
                (of_string
                   "46937548361816886563847943541604243346921840144669283775907349952397536703416"));
            Base.(to_bytes (negate one)) ]
    end) *)

module AffineWeierstrass =
  Ec.MakeAffineWeierstrass (Base) (Scalar)
    (struct
      let a = Base.(negate (of_string "3763200000"))

      let b = Base.(negate (of_string "78675968000000"))

      let cofactor = Z.of_int 4

      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [ Base.(
              to_bytes
                (of_string
                   "4732093294267640299242820317528400560681136891967543338160850811774078125840"));
            Base.(
              to_bytes
                (of_string
                   "31127102290931869693084292284935581507759552409643462510093198106308390504714"))
          ]
    end)

(* let from_twisted_to_montgomery p =
  Ec.from_twisted_to_montgomery
    (module AffineEdwards)
    (module AffineMontgomery)
    p

let from_montgomery_to_twisted p =
  Ec.from_montgomery_to_twisted
    (module AffineMontgomery)
    (module AffineEdwards)
    p *)

(* let from_montgomery_to_weierstrass p =
  Ec.from_montgomery_to_weierstrass
    (module AffineMontgomery)
    (module AffineWeierstrass)
    p *)

(* let from_twisted_to_weierstrass p =
  let p = from_twisted_to_montgomery p in
  Option.bind p (fun opt ->
      Ec.from_montgomery_to_weierstrass
        (module AffineMontgomery)
        (module AffineWeierstrass)
        opt) *)
