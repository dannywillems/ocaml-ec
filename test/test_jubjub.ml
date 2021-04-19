module ValueGeneration = Ec_pbt.MakeValueGeneration (Ec_jubjub.Affine)
module Equality = Ec_pbt.MakeEquality (Ec_jubjub.Affine)
module Properties = Ec_pbt.MakeECProperties (Ec_jubjub.Affine)
module EdwardsCurveProperties =
  Ec_pbt.MakeEdwardsCurveProperties (Ec_jubjub.Affine)

let test_random_is_not_small_order () =
  Ec_pbt.repeat 1000 (fun () ->
      assert (not Ec_jubjub.Affine.(is_small_order (random ()))))

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Jubjub"
    [ ( "Vectors",
        [ Alcotest.test_case
            "test random elements are in the prime subgroup"
            `Quick
            (test_random_is_not_small_order ()) ] );
      ValueGeneration.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Properties.get_tests ();
      Equality.get_tests () ]
