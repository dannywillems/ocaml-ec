open Core
open Core_bench
open Mec.Hash

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

module Poseidon = Poseidon252.Make (Scalar)

let t1 =
  let n = 3 in
  let inputs = Array.init n ~f:(fun _i -> Scalar.random ()) in
  let name =
    Printf.sprintf
      "Benchmark one permutation of Poseidon252 with ocaml-ff backend on an \
       input of %d elements"
      n
  in
  Bench.Test.create ~name (fun () ->
      let ctxt = Poseidon.Strategy.init inputs in
      let () = Poseidon.Strategy.apply_perm ctxt in
      let _v = Poseidon.Strategy.get ctxt in
      ())

let command = Bench.make_command [t1]

let () = Core.Command.run command
