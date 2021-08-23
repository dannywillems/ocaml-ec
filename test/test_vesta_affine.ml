module VestaValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.Vesta.Affine)
module VestaEquality = Mec.Curve.PBT.MakeEquality (Mec.Curve.Vesta.Affine)
module VestaECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.Vesta.Affine)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Vesta affine coordinates"
    [ VestaValueGeneration.get_tests ();
      VestaEquality.get_tests ();
      VestaECProperties.get_tests () ]
