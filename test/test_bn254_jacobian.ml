module BN254JacobianValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.BN254.Jacobian)
module BN254JacobianEquality =
  Mec.Curve.PBT.MakeEquality (Mec.Curve.BN254.Jacobian)
module BN254JacobianECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.BN254.Jacobian)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "BN254 jacobian coordinates"
    [ BN254JacobianValueGeneration.get_tests ();
      BN254JacobianEquality.get_tests ();
      BN254JacobianECProperties.get_tests () ]
