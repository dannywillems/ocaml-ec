module Scalar = Bls12_381.Fr

module Poseidon = Poseidon252.Make (Scalar)

let () =
  let n = 5 in
  let inputs = Array.init n (fun _i -> Scalar.random ()) in
  let ctxt = Poseidon.Hash.init () in
  let ctxt = Poseidon.Hash.hash ctxt inputs in
  let _v = Poseidon.Hash.get ctxt in
  ()
