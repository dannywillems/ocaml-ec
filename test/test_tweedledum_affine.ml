module TweedledumValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.Tweedledum.Affine)
module TweedledumEquality =
  Mec.Curve.PBT.MakeEquality (Mec.Curve.Tweedledum.Affine)
module TweedledumECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.Tweedledum.Affine)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Tweedledum affine coordinates"
    [ TweedledumValueGeneration.get_tests ();
      TweedledumEquality.get_tests ();
      TweedledumECProperties.get_tests () ]
