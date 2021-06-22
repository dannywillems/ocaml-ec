module IsoPallasValueGeneration =
  Ec_pbt.MakeValueGeneration (Ec_pallas.Iso.Projective)
module IsoPallasEquality = Ec_pbt.MakeEquality (Ec_pallas.Iso.Projective)
module IsoPallasECProperties = Ec_pbt.MakeECProperties (Ec_pallas.Iso.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Iso Pallas"
    [ IsoPallasValueGeneration.get_tests ();
      IsoPallasEquality.get_tests ();
      IsoPallasECProperties.get_tests () ]
