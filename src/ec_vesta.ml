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
