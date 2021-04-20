module ValueGeneration =
  Ec_pbt.MakeValueGeneration (Ec_babyjubjub_reduced.Affine)
module Equality = Ec_pbt.MakeEquality (Ec_babyjubjub_reduced.Affine)
module Properties = Ec_pbt.MakeECProperties (Ec_babyjubjub_reduced.Affine)
module EdwardsCurveProperties =
  Ec_pbt.MakeEdwardsCurveProperties (Ec_babyjubjub_reduced.Affine)

let test_random_is_not_small_order () =
  assert (not Ec_babyjubjub_reduced.Affine.(is_small_order (random ())))

let test_random_points_not_on_curve () =
  (* pick random values u and v and test constructors fail *)
  let u = Ec_babyjubjub_reduced.Affine.BaseField.random () in
  let v = Ec_babyjubjub_reduced.Affine.BaseField.random () in
  let bytes =
    Bytes.concat
      Bytes.empty
      [ Ec_babyjubjub_reduced.Affine.BaseField.to_bytes u;
        Ec_babyjubjub_reduced.Affine.BaseField.to_bytes v ]
  in
  (* check_bytes *)
  assert (not (Ec_babyjubjub_reduced.Affine.check_bytes bytes)) ;
  (* of_bytes_opt *)
  assert (Option.is_none (Ec_babyjubjub_reduced.Affine.of_bytes_opt bytes)) ;
  (* of_bytes_exn *)
  ( try
      ignore (Ec_babyjubjub_reduced.Affine.of_bytes_exn bytes) ;
      assert false
    with
  | Ec_babyjubjub_reduced.Affine.Not_on_curve _ -> ()
  | _ -> assert false ) ;
  (* from_coordinates_opt *)
  assert (
    Option.is_none (Ec_babyjubjub_reduced.Affine.from_coordinates_opt ~u ~v) ) ;
  (* from_coordinates_exn *)
  try
    ignore (Ec_babyjubjub_reduced.Affine.from_coordinates_exn ~u ~v) ;
    assert false
  with
  | Ec_babyjubjub_reduced.Affine.Not_on_curve _ -> ()
  | _ -> assert false

let () =
  let open Alcotest in
  run
    ~verbose:true
    "BabyJubjub reduced twisted edwards form"
    [ ( "Vectors",
        [ Alcotest.test_case
            "test random elements are in the prime subgroup"
            `Quick
            (Ec_pbt.repeat 100 test_random_is_not_small_order);
          Alcotest.test_case
            "test random coordinates u, v do not give a point on the curve"
            `Quick
            (Ec_pbt.repeat 100 test_random_points_not_on_curve) ] );
      ValueGeneration.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Properties.get_tests ();
      Equality.get_tests () ]
