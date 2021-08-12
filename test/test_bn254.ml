module BN254ValueGeneration = Ec_pbt.MakeValueGeneration (Ec_bn254.Projective)
module BN254Equality = Ec_pbt.MakeEquality (Ec_bn254.Projective)
module BN254ECProperties = Ec_pbt.MakeECProperties (Ec_bn254.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "bn254"
    [ BN254ValueGeneration.get_tests ();
      BN254Equality.get_tests ();
      BN254ECProperties.get_tests () ]
