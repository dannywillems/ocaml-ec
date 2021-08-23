module TweedledeeValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.Tweedledee.Affine)
module TweedledeeEquality =
  Mec.Curve.PBT.MakeEquality (Mec.Curve.Tweedledee.Affine)
module TweedledeeECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.Tweedledee.Affine)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Tweedledee affine coordinates"
    [ TweedledeeValueGeneration.get_tests ();
      TweedledeeEquality.get_tests ();
      TweedledeeECProperties.get_tests () ]
