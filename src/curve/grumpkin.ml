(**
   https://neuromancer.sk/std/bn/bn254
   https://tools.ietf.org/id/draft-yonezawa-pairing-friendly-curves-00.html

  Scalar field: 16798108731015832284940804142231733909889187121439069848933715426072753864723 (254 bits - 32 bytes)
  Base field: 16798108731015832284940804142231733909759579603404752749028378864165570215949 (254 bits - 32 bytes)

  Base field multiplicative subgroup decomposition:
    TODO
  Prime field multiplication subgroup decomposition:
    TODO
*)

module Fq = Bn254.Affine.Scalar
module Fp = Bn254.Affine.Base

module Projective =
  Ec.MakeProjectiveWeierstrass (Fq) (Fp)
    (struct
      let a = Fq.zero

      let b = Fq.(negate (of_string "17"))

      let cofactor = Z.one

      (* x = 0x2523648240000001BA344D80000000086121000000000013A700000000000012
         y = 0x0000000000000000000000000000000000000000000000000000000000000001
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [ Fq.(to_bytes (of_string "1"));
            Fq.(
              to_bytes
                (of_string
                   "17631683881184975370165255887551781615748388533673675138860"));
            Fq.(to_bytes one) ]
    end)

module Jacobian =
  Ec.MakeJacobianWeierstrass (Fq) (Fp)
    (struct
      let a = Fq.zero

      let b = Fq.(negate (of_string "17"))

      let cofactor = Z.one

      (* x = 1
         y = \sqrt(-16)
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [ Fq.(to_bytes (of_string "1"));
            Fq.(
              to_bytes
                (of_string
                   "17631683881184975370165255887551781615748388533673675138860"));
            Fq.(to_bytes one) ]
    end)

module Affine =
  Ec.MakeAffineWeierstrass (Fq) (Fp)
    (struct
      let a = Fq.zero

      let b = Fq.(negate (of_string "17"))

      let cofactor = Z.one

      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [ Fq.(to_bytes (of_string "1"));
            Fq.(
              to_bytes
                (of_string
                   "17631683881184975370165255887551781615748388533673675138860"))
          ]
    end)

(* let from_affine_weierstrass_to_jacobian_weierstrass p = *)
(*   Ec.from_affine_weierstrass_to_jacobian_weierstrass *)
(*     (module Affine) *)
(*     (module Jacobian) *)
(*     p *)

(* let from_affine_weierstrass_to_projective_weierstrass p = *)
(*   Ec.from_affine_weierstrass_to_projective_weierstrass *)
(*     (module Affine) *)
(*     (module Projective) *)
(*     p *)

(* let from_jacobian_weierstrass_to_affine_weierstrass p = *)
(*   Ec.from_jacobian_weierstrass_to_affine_weierstrass *)
(*     (module Jacobian) *)
(*     (module Affine) *)
(*     p *)

(* let from_projective_weierstrass_to_affine_weierstrass p = *)
(*   Ec.from_projective_weierstrass_to_affine_weierstrass *)
(*     (module Projective) *)
(*     (module Affine) *)
(*     p *)
