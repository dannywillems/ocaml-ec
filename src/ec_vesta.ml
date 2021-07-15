(**
  Base field: 2^254 + 45560315531506369815346746415080538113 = 28948022309329048855892746252171976963363056481941647379679742748393362948097
  Scalar field: 2^254 + 45560315531419706090280762371685220353 = 28948022309329048855892746252171976963363056481941560715954676764349967630337

  Base field multiplicative subgroup decomposition:
    2^32 * 3^2 * 1709 * 24859 * 17627503553531704781201602214972145569028026719617221564519

  Prime field multiplication subgroup decomposition:
    2^32 * 3 * 463 * 4852402207910482324454106387152561316357015077916052529702775169
*)

let two_z = Z.succ Z.one

module Fq = Ff.MakeFp (struct
  let prime_order =
    Z.((two_z ** 254) + Z.of_string "45560315531506369815346746415080538113")
end)

module Fp = Ff.MakeFp (struct
  let prime_order =
    Z.((two_z ** 254) + Z.of_string "45560315531419706090280762371685220353")
end)

module Projective =
  Ec.MakeProjectiveWeierstrass (Fq) (Fp)
    (struct
      (* https://github.com/zcash/pasta *)
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
