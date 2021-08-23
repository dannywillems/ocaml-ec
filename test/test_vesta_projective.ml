module VestaValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.Vesta.Projective)
module VestaEquality =
  Mec.Curve.PBT.MakeEquality (Mec.Curve.Vesta.Projective)
module VestaECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.Vesta.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Vesta projective coordinates"
    [ VestaValueGeneration.get_tests ();
      VestaEquality.get_tests ();
      VestaECProperties.get_tests () ]
