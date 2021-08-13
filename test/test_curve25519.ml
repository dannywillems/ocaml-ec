module ValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Curve25519.Affine)
module Equality = Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Curve25519.Affine)
module Properties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Curve25519.Affine)
module EdwardsCurveProperties =
  Mec.Curve.Utils.PBT.MakeEdwardsCurveProperties (Mec.Curve.Curve25519.Affine)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Curve25519"
    [ ValueGeneration.get_tests ();
      Properties.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Equality.get_tests () ]
