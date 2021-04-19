module ValueGeneration =
  Ec_pbt.MakeValueGeneration (Ec_babyjubjub_reduced.Affine)
module Equality = Ec_pbt.MakeEquality (Ec_babyjubjub_reduced.Affine)
module Properties = Ec_pbt.MakeECProperties (Ec_babyjubjub_reduced.Affine)
module EdwardsCurveProperties =
  Ec_pbt.MakeEdwardsCurveProperties (Ec_babyjubjub_reduced.Affine)

let test_random_is_not_small_order () =
  Ec_pbt.repeat 1000 (fun () ->
      assert (not Ec_babyjubjub_reduced.Affine.(is_small_order (random ()))))

let () =
  let open Alcotest in
  run
    ~verbose:true
    "BabyJubjub reduced twisted edwards form"
    [ ( "Vectors",
        [ Alcotest.test_case
            "test random elements are in the prime subgroup"
            `Quick
            (test_random_is_not_small_order ()) ] );
      ValueGeneration.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Properties.get_tests ();
      Equality.get_tests () ]
