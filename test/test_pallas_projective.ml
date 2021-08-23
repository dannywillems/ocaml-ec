module PallasValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.Pallas.Projective)
module PallasEquality =
  Mec.Curve.PBT.MakeEquality (Mec.Curve.Pallas.Projective)
module PallasECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.Pallas.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Pallas projective coordinates"
    [ PallasValueGeneration.get_tests ();
      PallasEquality.get_tests ();
      PallasECProperties.get_tests () ]
