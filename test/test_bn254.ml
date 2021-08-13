module BN254ValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.BN254.Projective)
module BN254Equality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.BN254.Projective)
module BN254ECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.BN254.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "bn254"
    [ BN254ValueGeneration.get_tests ();
      BN254Equality.get_tests ();
      BN254ECProperties.get_tests () ]
