open Mec.Hash

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.(
      of_string
        "52435875175126190479447740508185965837690552500527637822603658699938581184513")
end)

module Poseidon = PoseidonFull.Make (Scalar)

let test_perm_is_consistent () =
  let x = Array.make PoseidonFull.Constant.width (Scalar.of_string "17") in
  let y = Array.make PoseidonFull.Constant.width (Scalar.of_string "17") in
  let z = Array.make PoseidonFull.Constant.width (Scalar.of_string "19") in

  let state_x = Poseidon.Strategy.init x in
  let state_y = Poseidon.Strategy.init y in
  let state_z = Poseidon.Strategy.init z in

  Poseidon.Strategy.apply_perm state_x ;
  Poseidon.Strategy.apply_perm state_y ;
  Poseidon.Strategy.apply_perm state_z ;

  assert (Poseidon.Strategy.(get state_x = get state_y)) ;
  assert (Poseidon.Strategy.(get state_x <> get state_z))

let () =
  Alcotest.run
    ~verbose:true
    "PoseidonFull"
    [ ( "Properties",
        [Alcotest.test_case "Perm is consistent" `Quick test_perm_is_consistent]
      ) ]
