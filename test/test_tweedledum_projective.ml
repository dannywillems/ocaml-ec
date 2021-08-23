module TweedledumValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.Tweedledum.Projective)
module TweedledumEquality =
  Mec.Curve.PBT.MakeEquality (Mec.Curve.Tweedledum.Projective)
module TweedledumECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.Tweedledum.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Tweedledum projective coordinates"
    [ TweedledumValueGeneration.get_tests ();
      TweedledumEquality.get_tests ();
      TweedledumECProperties.get_tests () ]
