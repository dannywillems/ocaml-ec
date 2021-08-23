module Secp256r1ValueGeneration =
  Mec.Curve.PBT.MakeValueGeneration (Mec.Curve.Secp256r1.Affine)
module Secp256r1Equality =
  Mec.Curve.PBT.MakeEquality (Mec.Curve.Secp256r1.Affine)
module Secp256r1ECProperties =
  Mec.Curve.PBT.MakeECProperties (Mec.Curve.Secp256r1.Affine)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "secp256r1 affine coordinates"
    [ Secp256r1ValueGeneration.get_tests ();
      Secp256r1Equality.get_tests ();
      Secp256r1ECProperties.get_tests () ]
