module TweedledumValueGeneration =
  Ec_pbt.MakeValueGeneration (Ec_tweedledum.Projective)
module TweedledumEquality = Ec_pbt.MakeEquality (Ec_tweedledum.Projective)
module TweedledumECProperties =
  Ec_pbt.MakeECProperties (Ec_tweedledum.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Tweedledum"
    [ TweedledumValueGeneration.get_tests ();
      TweedledumEquality.get_tests ();
      TweedledumECProperties.get_tests () ]
