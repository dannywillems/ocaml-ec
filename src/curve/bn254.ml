(**
   https://neuromancer.sk/std/bn/bn254
   https://tools.ietf.org/id/draft-yonezawa-pairing-friendly-curves-00.html

  Base field: 16798108731015832284940804142231733909889187121439069848933715426072753864723 (253 bits - 32 bytes)
  Scalar field: 16798108731015832284940804142231733909759579603404752749028378864165570215949 (253 bits - 32 bytes)

  Base field multiplicative subgroup decomposition:
    TODO
  Prime field multiplication subgroup decomposition:
    TODO
*)

module Fq = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "0x2523648240000001BA344D80000000086121000000000013A700000000000013"
end)

module Fp = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "0x2523648240000001BA344D8000000007FF9F800000000010A10000000000000D"
end)

module Projective =
  Ec.MakeProjectiveWeierstrass (Fq) (Fp)
    (struct
      let a = Fq.zero

      let b = Fq.of_z (Z.of_int 2)

      (* x = 0x2523648240000001BA344D80000000086121000000000013A700000000000012
         y = 0x0000000000000000000000000000000000000000000000000000000000000001
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [ Fq.(
              to_bytes
                (of_string
                   "0x2523648240000001BA344D80000000086121000000000013A700000000000012"));
            Fq.(to_bytes (of_string "1"));
            Fq.(to_bytes one) ]
    end)

module Affine =
  Ec.MakeAffineWeierstrass (Fq) (Fp)
    (struct
      let a = Fq.zero

      let b = Fq.of_z (Z.of_int 2)

      (* x = 0x2523648240000001BA344D80000000086121000000000013A700000000000012
         y = 0x0000000000000000000000000000000000000000000000000000000000000001
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [ Fq.(
              to_bytes
                (of_string
                   "0x2523648240000001BA344D80000000086121000000000013A700000000000012"));
            Fq.(to_bytes (of_string "1")) ]
    end)
