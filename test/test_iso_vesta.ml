module IsoVestaValueGeneration =
  Ec_pbt.MakeValueGeneration (Ec_vesta.Iso.Projective)
module IsoVestaEquality = Ec_pbt.MakeEquality (Ec_vesta.Iso.Projective)
module IsoVestaECProperties = Ec_pbt.MakeECProperties (Ec_vesta.Iso.Projective)

let test_isogeny () =
  let r = Ec_vesta.Iso.Projective.random () in
  ignore @@ Ec_vesta.iso_map r

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Iso Vesta"
    [ (* ("Test isogeny", [test_case "With random point" `Quick test_isogeny]); *)
      IsoVestaValueGeneration.get_tests ();
      IsoVestaEquality.get_tests ();
      IsoVestaECProperties.get_tests () ]
