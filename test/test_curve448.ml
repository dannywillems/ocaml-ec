module ValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.Curve448.Affine)
module Equality = Mec.Curve.PBT.MakeEquality (Mec.Curve.Curve448.Affine)
module Properties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.Curve448.Affine)
module EdwardsCurveProperties =
  Mec.Curve.PBT.MakeEdwardsCurveProperties (Mec.Curve.Curve448.Affine)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Curve448"
    [ ValueGeneration.get_tests ();
      Properties.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Equality.get_tests () ]
