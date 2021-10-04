open Core
open Core_bench
open Mec.Hash

(* module Scalar = Ff.MakeFp (struct
 *   let prime_order =
 *     Z.of_string
 *       "52435875175126190479447740508185965837690552500527637822603658699938581184513"
 * end) *)

module Scalar = Bls12_381.Fr
module Poseidon = PoseidonFull.Make (Scalar)

let t1 =
  let n = 1 in
  let inputs = Array.init n ~f:(fun _i -> Scalar.random ()) in
  Bench.Test.create
    ~name:"Benchmark PoseidonFull with bls12-381-unix backend"
    (fun () ->
      let ctxt = Poseidon.Hash.init () in
      let ctxt = Poseidon.Hash.digest ctxt inputs in
      let _v = Poseidon.Hash.get ctxt in
      ())

let command = Bench.make_command [t1]

let () = Core.Command.run command
