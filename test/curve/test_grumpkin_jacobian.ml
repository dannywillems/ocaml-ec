module GrumpkinJacobianValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Grumpkin.Jacobian)
module GrumpkinJacobianEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Grumpkin.Jacobian)
module GrumpkinJacobianECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Grumpkin.Jacobian)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Grumpkin jacobian coordinates"
    [ GrumpkinJacobianValueGeneration.get_tests ();
      GrumpkinJacobianEquality.get_tests ();
      GrumpkinJacobianECProperties.get_tests () ]
