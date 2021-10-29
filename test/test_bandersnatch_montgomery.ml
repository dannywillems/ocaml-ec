module ValueGeneration_Mt =
  Mec.Curve.Utils.PBT.MakeValueGeneration
    (Mec.Curve.Bandersnatch.AffineMontgomery)
module Equality_Mt =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Bandersnatch.AffineMontgomery)
module Properties_Mt =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Bandersnatch.AffineMontgomery)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Bandersnatch"
    [ ValueGeneration_Mt.get_tests ();
      Properties_Mt.get_tests ();
      Equality_Mt.get_tests () ]
