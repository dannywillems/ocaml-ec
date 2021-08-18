module TweedledumValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Tweedledum.Affine)
module TweedledumEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Tweedledum.Affine)
module TweedledumECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Tweedledum.Affine)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Tweedledum affine form"
    [ TweedledumValueGeneration.get_tests ();
      TweedledumEquality.get_tests ();
      TweedledumECProperties.get_tests () ]
