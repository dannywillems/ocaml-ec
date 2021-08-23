module VestaValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.Vesta.Jacobian)
module VestaEquality =
  Mec.Curve.PBT.MakeEquality (Mec.Curve.Vesta.Jacobian)
module VestaECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.Vesta.Jacobian)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Vesta jacobian coordinates"
    [ VestaValueGeneration.get_tests ();
      VestaEquality.get_tests ();
      VestaECProperties.get_tests () ]
