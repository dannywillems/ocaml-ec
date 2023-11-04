module GrumpkinAffineValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Grumpkin.Affine)
module GrumpkinAffineEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Grumpkin.Affine)
module GrumpkinAffineECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Grumpkin.Affine)
module GrumpkinAffineRepresentation =
  Mec.Curve.Utils.PBT.MakeCompressedSerialisationAffine
    (Mec.Curve.Grumpkin.Affine)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Grumpkin affine form"
    [ GrumpkinAffineValueGeneration.get_tests ();
      GrumpkinAffineEquality.get_tests ();
      GrumpkinAffineECProperties.get_tests ();
      GrumpkinAffineRepresentation.get_tests () ]
