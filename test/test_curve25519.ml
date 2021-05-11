module ValueGeneration = Ec_pbt.MakeValueGeneration (Ec_curve25519.Affine)
module Equality = Ec_pbt.MakeEquality (Ec_curve25519.Affine)
module Properties = Ec_pbt.MakeECProperties (Ec_curve25519.Affine)
module EdwardsCurveProperties =
  Ec_pbt.MakeEdwardsCurveProperties (Ec_curve25519.Affine)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Curve25519"
    [ ValueGeneration.get_tests ();
      Properties.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Equality.get_tests () ]
