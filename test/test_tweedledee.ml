module TweedledeeValueGeneration =
  Ec_pbt.MakeValueGeneration (Ec_tweedledee.Projective)
module TweedledeeEquality = Ec_pbt.MakeEquality (Ec_tweedledee.Projective)
module TweedledeeECProperties =
  Ec_pbt.MakeECProperties (Ec_tweedledee.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Tweedledee"
    [ TweedledeeValueGeneration.get_tests ();
      TweedledeeEquality.get_tests ();
      TweedledeeECProperties.get_tests () ]
