open Mec.Hash

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.(
      ((one + one) ** 254)
      + Z.of_string "45560315531419706090280762371685220353")
end)

module Poseidon = Poseidon128.Make (Scalar)
module PoseidonNopad = PoseidonNopad128.Make (Scalar)

let test_random_inputs () =
  List.iter
    (fun _ ->
      let x = Scalar.random () in
      let ctxt = Poseidon.Hash.init () in
      let ctxt = Poseidon.Hash.digest ctxt [| x |] in
      let v = Poseidon.Hash.get ctxt in

      let ctxt = PoseidonNopad.Hash.init () in
      let ctxt = PoseidonNopad.Hash.digest ctxt [| x; Scalar.one |] in
      let v' = PoseidonNopad.Hash.get ctxt in
      assert (Scalar.eq v v'))
    (List.init 10 (fun _i -> _i))

let () =
  Alcotest.run
    ~verbose:true
    "PoseidonNopad"
    [ ( "Consistency with Orchard",
        [ Alcotest.test_case
            "Random inputs of length 1"
            `Quick
            test_random_inputs ] ) ]
