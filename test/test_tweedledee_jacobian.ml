module TweedledeeValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.Tweedledee.Jacobian)
module TweedledeeEquality =
  Mec.Curve.PBT.MakeEquality (Mec.Curve.Tweedledee.Jacobian)
module TweedledeeECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.Tweedledee.Jacobian)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Tweedledee jacobian coordinates"
    [ TweedledeeValueGeneration.get_tests ();
      TweedledeeEquality.get_tests ();
      TweedledeeECProperties.get_tests () ]
