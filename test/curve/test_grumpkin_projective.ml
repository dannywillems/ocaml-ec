module GrumpkinProjectiveValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Grumpkin.Projective)
module GrumpkinProjectiveEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Grumpkin.Projective)
module GrumpkinProjectiveECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Grumpkin.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Grumpkin projective form"
    [ GrumpkinProjectiveValueGeneration.get_tests ();
      GrumpkinProjectiveEquality.get_tests ();
      GrumpkinProjectiveECProperties.get_tests () ]
