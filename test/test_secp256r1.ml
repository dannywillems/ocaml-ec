module Secp256r1ValueGeneration =
  Ec_pbt.MakeValueGeneration (Ec_secp256r1.Projective)
module Secp256r1Equality = Ec_pbt.MakeEquality (Ec_secp256r1.Projective)
module Secp256r1ECProperties = Ec_pbt.MakeECProperties (Ec_secp256r1.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "secp256r1"
    [ Secp256r1ValueGeneration.get_tests ();
      Secp256r1Equality.get_tests ();
      Secp256r1ECProperties.get_tests () ]
