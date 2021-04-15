module VestaValueGeneration = Ec_pbt.MakeValueGeneration (Ec_vesta.Projective)
module VestaEquality = Ec_pbt.MakeEquality (Ec_vesta.Projective)
module VestaECProperties = Ec_pbt.MakeECProperties (Ec_vesta.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Vesta"
    [ VestaValueGeneration.get_tests ();
      VestaEquality.get_tests ();
      VestaECProperties.get_tests () ]
