module PallasValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.Pallas.Affine)
module PallasEquality =
  Mec.Curve.PBT.MakeEquality (Mec.Curve.Pallas.Affine)
module PallasECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.Pallas.Affine)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Pallas affine coordinates"
    [ PallasValueGeneration.get_tests ();
      PallasEquality.get_tests ();
      PallasECProperties.get_tests () ]
