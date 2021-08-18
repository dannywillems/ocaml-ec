module TweedledeeValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Tweedledee.Affine)
module TweedledeeEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Tweedledee.Affine)
module TweedledeeECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Tweedledee.Affine)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Tweedledee affine form"
    [ TweedledeeValueGeneration.get_tests ();
      TweedledeeEquality.get_tests ();
      TweedledeeECProperties.get_tests () ]
