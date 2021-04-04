module ValueGeneration =
  Ec_pbt.MakeValueGeneration (Ec_babyjubjub.ReducedTwistedEdwards)
module Equality = Ec_pbt.MakeEquality (Ec_babyjubjub.ReducedTwistedEdwards)
module Properties = Ec_pbt.MakeECProperties (Ec_babyjubjub.ReducedTwistedEdwards)

(* let () =
 *   let open Alcotest in
 *   run
 *     ~verbose:true
 *     "BabyJubjub reduced twisted edwards form"
 *     [ ValueGeneration.get_tests ();
 *       Properties.get_tests ();
 *       Equality.get_tests () ] *)
