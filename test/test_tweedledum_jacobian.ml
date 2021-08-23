module TweedledumValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.Tweedledum.Jacobian)
module TweedledumEquality =
  Mec.Curve.PBT.MakeEquality (Mec.Curve.Tweedledum.Jacobian)
module TweedledumECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.Tweedledum.Jacobian)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Tweedledum jacobian coordinates"
    [ TweedledumValueGeneration.get_tests ();
      TweedledumEquality.get_tests ();
      TweedledumECProperties.get_tests () ]
