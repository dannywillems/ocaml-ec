module PallasValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.Pallas.Jacobian)
module PallasEquality =
  Mec.Curve.PBT.MakeEquality (Mec.Curve.Pallas.Jacobian)
module PallasECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.Pallas.Jacobian)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Pallas jacobian coordinates"
    [ PallasValueGeneration.get_tests ();
      PallasEquality.get_tests ();
      PallasECProperties.get_tests () ]
