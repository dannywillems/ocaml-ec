module G1ValueGeneration =
  Ec_pbt.MakeValueGeneration (Ec_bls12_381.G1.Projective)
module G1Equality = Ec_pbt.MakeEquality (Ec_bls12_381.G1.Projective)
module G1ECProperties = Ec_pbt.MakeECProperties (Ec_bls12_381.G1.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "BLS12-381 G1"
    [ G1ValueGeneration.get_tests ();
      G1Equality.get_tests ();
      G1ECProperties.get_tests () ]
