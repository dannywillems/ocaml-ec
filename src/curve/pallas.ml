(**
  Base field: 2^254 + 45560315531419706090280762371685220353 = 28948022309329048855892746252171976963363056481941560715954676764349967630337 (254 bits - 32 bytes)
  Scalar field: 2^254 + 45560315531506369815346746415080538113 = 28948022309329048855892746252171976963363056481941647379679742748393362948097 (254 bits - 32 bytes)

  Base field multiplicative subgroup decomposition:
    2^32 * 3 * 463 * 4852402207910482324454106387152561316357015077916052529702775169

  Prime field multiplication subgroup decomposition:
    2^32 * 3^2 * 1709 * 24859 * 17627503553531704781201602214972145569028026719617221564519
*)

let two_z = Z.succ Z.one

module Fq = Ff.MakeFp (struct
  let prime_order =
    Z.((two_z ** 254) + Z.of_string "45560315531419706090280762371685220353")
end)

module Fp = Ff.MakeFp (struct
  let prime_order =
    Z.((two_z ** 254) + Z.of_string "45560315531506369815346746415080538113")
end)

module Projective =
  Ec.MakeProjectiveWeierstrass (Fq) (Fp)
    (struct
      (* https://github.com/zcash/pasta *)
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

module Jacobian =
  Ec.MakeJacobianWeierstrass (Fq) (Fp)
    (struct
      (* https://github.com/zcash/pasta *)
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
      (* https://github.com/zcash/pasta *)
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

let from_affine_to_jacobian p =
  Ec.from_affine_to_jacobian (module Affine) (module Jacobian) p

let from_affine_to_projective p =
  Ec.from_affine_to_projective (module Affine) (module Projective) p

let from_jacobian_to_affine p =
  Ec.from_jacobian_to_affine (module Jacobian) (module Affine) p

let from_projective_to_affine p =
  Ec.from_projective_to_affine (module Projective) (module Affine) p

module Iso = struct
  module Affine =
    Ec.MakeAffineWeierstrass (Fq) (Fp)
      (struct
        let a =
          Fq.of_string
            "0x18354a2eb0ea8c9c49be2d7258370742b74134581a27a59f92bb4b0b657a014b"

        let b = Fq.of_string "1265"

        let cofactor = Z.one

        let bytes_generator =
          Bytes.concat
            Bytes.empty
            [ Fq.(to_bytes zero);
              Fq.(
                to_bytes
                  (of_string
                     "10190879275902416739536393627353788808482399662677727499756286083144305497533"))
            ]
      end)
end

let csts_iso_map =
  [| Fq.of_string
       "0x0e38e38e38e38e38e38e38e38e38e38e4081775473d8375b775f6034aaaaaaab";
     Fq.of_string
       "0x3509afd51872d88e267c7ffa51cf412a0f93b82ee4b994958cf863b02814fb76";
     Fq.of_string
       "0x17329b9ec525375398c7d7ac3d98fd13380af066cfeb6d690eb64faef37ea4f7";
     Fq.of_string
       "0x1c71c71c71c71c71c71c71c71c71c71c8102eea8e7b06eb6eebec06955555580";
     Fq.of_string
       "0x1d572e7ddc099cff5a607fcce0494a799c434ac1c96b6980c47f2ab668bcd71f";
     Fq.of_string
       "0x325669becaecd5d11d13bf2a7f22b105b4abf9fb9a1fc81c2aa3af1eae5b6604";
     Fq.of_string
       "0x1a12f684bda12f684bda12f684bda12f7642b01ad461bad25ad985b5e38e38e4";
     Fq.of_string
       "0x1a84d7ea8c396c47133e3ffd28e7a09507c9dc17725cca4ac67c31d8140a7dbb";
     Fq.of_string
       "0x3fb98ff0d2ddcadd303216cce1db9ff11765e924f745937802e2be87d225b234";
     Fq.of_string
       "0x025ed097b425ed097b425ed097b425ed0ac03e8e134eb3e493e53ab371c71c4f";
     Fq.of_string
       "0x0c02c5bcca0e6b7f0790bfb3506defb65941a3a4a97aa1b35a28279b1d1b42ae";
     Fq.of_string
       "0x17033d3c60c68173573b3d7f7d681310d976bbfabbc5661d4d90ab820b12320a";
     Fq.of_string
       "0x40000000000000000000000000000000224698fc094cf91b992d30ecfffffde5"
  |]

(* See 5.4.9.8: Group Hash into Pallas and Vesta *)
let iso_map p =
  if Iso.Affine.is_zero p then Affine.zero
  else
    let x = Iso.Affine.get_x_coordinate p in
    let y = Iso.Affine.get_y_coordinate p in
    let xx = Fq.(x * x) in
    let xxx = Fq.(xx * x) in
    let x' =
      Fq.(
        ( (csts_iso_map.(0) * xxx)
        + (csts_iso_map.(1) * xx)
        + (csts_iso_map.(2) * x)
        + csts_iso_map.(3) )
        / (xx + (csts_iso_map.(4) * x) + csts_iso_map.(5)))
    in
    let y' =
      Fq.(
        ( (csts_iso_map.(6) * xxx)
        + (csts_iso_map.(7) * xx)
        + (csts_iso_map.(8) * x)
        + csts_iso_map.(9) )
        * y
        / ( xxx
          + (csts_iso_map.(10) * xx)
          + (csts_iso_map.(11) * x)
          + csts_iso_map.(12) ))
    in
    Affine.from_coordinates_exn ~x:x' ~y:y'
