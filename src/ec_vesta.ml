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

(* 5.4.9.8: Group Hash into Pallas and Vesta *)
module Iso = struct
  module Projective =
    Ec.MakeProjectiveWeierstrass (Fq) (Fp)
      (struct
        let a =
          Fq.of_string
            "0x267f9b2ee592271a81639c4d96f787739673928c7d01b212c515ad7242eaa6b1"

        let b = Fq.of_string "1265"

        let bytes_generator =
          Bytes.concat
            Bytes.empty
            [ Fq.(to_bytes zero);
              Fq.(
                to_bytes
                  (of_string
                     "24168672240094656118439194445685110693789334875363060050166186932715309622324"));
              Fq.(to_bytes one) ]
      end)
end

let csts_iso_map =
  [| Fq.of_string
       "0x38e38e38e38e38e38e38e38e38e38e390205dd51cfa0961a43cd42c800000001";
     Fq.of_string
       "0x1d935247b4473d17acecf10f5f7c09a2216b8861ec72bd5d8b95c6aaf703bcc5";
     Fq.of_string
       "0x18760c7f7a9ad20ded7ee4a9cdf78f8fd59d03d23b39cb11aeac67bbeb586a3d";
     Fq.of_string
       "0x31c71c71c71c71c71c71c71c71c71c71e1c521a795ac8356fb539a6f0000002b";
     Fq.of_string
       "0x0a2de485568125d51454798a5b5c56b2a3ad678129b604d3b7284f7eaf21a2e9";
     Fq.of_string
       "0x14735171ee5427780c621de8b91c242a30cd6d53df49d235f169c187d2533465";
     Fq.of_string
       "0x12f684bda12f684bda12f684bda12f685601f4709a8adcb36bef1642aaaaaaab";
     Fq.of_string
       "0x2ec9a923da239e8bd6767887afbe04d121d910aefb03b31d8bee58e5fb81de63";
     Fq.of_string
       "0x19b0d87e16e2578866d1466e9de10e6497a3ca5c24e9ea634986913ab4443034";
     Fq.of_string
       "0x1ed097b425ed097b425ed097b425ed098bc32d36fb21a6a38f64842c55555533";
     Fq.of_string
       "0x2f44d6c801c1b8bf9e7eb64f890a820c06a767bfc35b5bac58dfecce86b2745e";
     Fq.of_string
       "0x3d59f455cafc7668252659ba2b546c7e926847fb9ddd76a1d43d449776f99d2f";
     Fq.of_string
       "0x40000000000000000000000000000000224698fc0994a8dd8c46eb20fffffde5"
  |]

(* See 5.4.9.8: Group Hash into Pallas and Vesta *)
let iso_map p =
  if Iso.Projective.is_zero p then Projective.zero
  else
    let x = Iso.Projective.get_affine_x_coordinate p in
    let y = Iso.Projective.get_affine_y_coordinate p in
    let xx = Fq.(x * x) in
    let xxx = Fq.(xx * x) in
    let x' =
      Fq.(
        ( (csts_iso_map.(1) * xxx)
        + (csts_iso_map.(2) * xx)
        + (csts_iso_map.(3) * x)
        + csts_iso_map.(4) )
        / (xx + (csts_iso_map.(5) * x) + csts_iso_map.(6)))
    in
    let y' =
      Fq.(
        ( (csts_iso_map.(7) * xxx)
        + (csts_iso_map.(8) * xx)
        + (csts_iso_map.(9) * x)
        + csts_iso_map.(10) )
        * y
        / ( xxx
          + (csts_iso_map.(11) * xx)
          + (csts_iso_map.(12) * x)
          + csts_iso_map.(13) ))
    in
    Projective.from_affine_coordinates_exn ~x:x' ~y:y'
