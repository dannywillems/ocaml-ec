module ValueGeneration =
  Ec_pbt.MakeValueGeneration (Ec_babyjubjub.ReducedTwistedEdwards)
module Equality = Ec_pbt.MakeEquality (Ec_babyjubjub.ReducedTwistedEdwards)
module Properties = Ec_pbt.MakeECProperties (Ec_babyjubjub.ReducedTwistedEdwards)
module EdwardsCurveProperties =
  Ec_pbt.MakeEdwardsCurveProperties (Ec_babyjubjub.ReducedTwistedEdwards)

let test_random_is_not_small_order () =
  Ec_pbt.repeat 1000 (fun () ->
      assert (
        not Ec_babyjubjub.ReducedTwistedEdwards.(is_small_order (random ())) ))

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
