let two_z = Z.succ Z.one

module Fq = Ff.MakeFp (struct
  let prime_order =
    Z.(
      (two_z ** 256) - (two_z ** 32) - (two_z ** 9) - (two_z ** 8)
      - (two_z ** 7) - (two_z ** 6) - (two_z ** 4) - one)
end)

module Fp = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "115792089237316195423570985008687907852837564279074904382605163141518161494337"
end)

module Projective =
  Ec.MakeProjectiveWeierstrass (Fq) (Fp)
    (struct
      (* See https://en.bitcoin.it/wiki/Secp256k1 *)
      let a = Fq.zero

      let b = Fq.of_z (Z.of_int 7)

      (* x = 55066263022277343669578718895168534326250603453777594175500187360389116729240
         y = 32670510020758816978083085130507043184471273380659243275938904335757337482424
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [ Fq.(
              to_bytes
                (of_string
                   "55066263022277343669578718895168534326250603453777594175500187360389116729240"));
            Fq.(
              to_bytes
                (of_string
                   "32670510020758816978083085130507043184471273380659243275938904335757337482424"));
            Fq.(to_bytes one) ]
    end)
