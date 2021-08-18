(* https://github.com/daira/tweedle *)

let two_z = Z.succ Z.one

module Fq = Ff.MakeFp (struct
  let prime_order =
    Z.((two_z ** 254) + Z.of_string "4707489545178046908921067385359695873")
end)

module Fp = Ff.MakeFp (struct
  let prime_order =
    Z.((two_z ** 254) + Z.of_string "4707489544292117082687961190295928833")
end)

module Projective =
  Ec.MakeProjectiveWeierstrass (Fq) (Fp)
    (struct
      (* https://github.com/daira/tweedle *)
      let a = Fq.zero

      let b = Fq.of_z (Z.of_int 5)

      let cofactor = Z.one

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

      let cofactor = Z.one

      (* x = -1
         y = 2
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [Fq.(to_bytes (negate (of_string "1"))); Fq.(to_bytes (of_string "2"))]
    end)
