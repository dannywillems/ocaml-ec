module PallasValueGeneration = Ec_pbt.MakeValueGeneration (Ec_pallas.Projective)
module PallasEquality = Ec_pbt.MakeEquality (Ec_pallas.Projective)
module PallasECProperties = Ec_pbt.MakeECProperties (Ec_pallas.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Pallas"
    [ PallasValueGeneration.get_tests ();
      PallasEquality.get_tests ();
      PallasECProperties.get_tests () ]
