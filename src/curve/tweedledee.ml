(**

  https://github.com/daira/tweedle

  Base field: 2^254 + 4707489544292117082687961190295928833 = 28948022309329048855892746252171976963322203655954433126947083963168578338817
  Scalar field: 2^254 + 4707489545178046908921067385359695873 = 28948022309329048855892746252171976963322203655955319056773317069363642105857

  Base field multiplicative subgroup decomposition:
    2^34 * 3 * 561665555565638329055562814312908972367531846121311209609791868583
  Prime field multiplication subgroup decomposition:
    2^33 * 3 * 5179 * 216901160674121772178243990852639108850176422522235334586122689
*)

let two_z = Z.succ Z.one

module Fq = Ff.MakeFp (struct
  let prime_order =
    Z.((two_z ** 254) + Z.of_string "4707489544292117082687961190295928833")
end)

module Fp = Ff.MakeFp (struct
  let prime_order =
    Z.((two_z ** 254) + Z.of_string "4707489545178046908921067385359695873")
end)

module Projective =
  Ec.MakeProjectiveWeierstrass (Fq) (Fp)
    (struct
      (* https://github.com/daira/tweedle *)
      let a = Fq.zero

      let b = Fq.of_z (Z.of_int 5)

      (* x = -1
         y = 2
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [ Fq.(to_bytes (negate (of_string "1")));
            Fq.(to_bytes (of_string "2"));
            Fq.(to_bytes one) ]
    end)

module Affine =
  Ec.MakeAffineWeierstrass (Fq) (Fp)
    (struct
      (* https://github.com/daira/tweedle *)
      let a = Fq.zero

      let b = Fq.of_z (Z.of_int 5)

      (* x = -1
         y = 2
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [Fq.(to_bytes (negate (of_string "1"))); Fq.(to_bytes (of_string "2"))]
    end)
