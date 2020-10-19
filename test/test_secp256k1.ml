module Secp256k1ValueGeneration =
  Ec_pbt.MakeValueGeneration (Ec_secp256k1.Projective)
module Secp256k1Equality = Ec_pbt.MakeEquality (Ec_secp256k1.Projective)
module Secp256k1ECProperties = Ec_pbt.MakeECProperties (Ec_secp256k1.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "secp256k1"
    [ Secp256k1ValueGeneration.get_tests ();
      Secp256k1Equality.get_tests ();
      Secp256k1ECProperties.get_tests () ]
