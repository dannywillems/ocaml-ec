module ValueGeneration = Ec_pbt.MakeValueGeneration (Ec_jubjub.TwistedEdwards)
module Equality = Ec_pbt.MakeEquality (Ec_jubjub.TwistedEdwards)
module Properties = Ec_pbt.MakeECProperties (Ec_jubjub.TwistedEdwards)

(* let () =
 *   let open Alcotest in
 *   run
 *     ~verbose:true
 *     "Jubjub"
 *     [ ValueGeneration.get_tests ();
 *       Properties.get_tests ();
 *       Equality.get_tests () ] *)
