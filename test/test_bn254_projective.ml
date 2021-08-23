module BN254ProjectiveValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.BN254.Projective)
module BN254ProjectiveEquality =
  Mec.Curve.PBT.MakeEquality (Mec.Curve.BN254.Projective)
module BN254ProjectiveECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.BN254.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "BN254 projective form"
    [ BN254ProjectiveValueGeneration.get_tests ();
      BN254ProjectiveEquality.get_tests ();
      BN254ProjectiveECProperties.get_tests () ]
