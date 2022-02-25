open Core
open Core_bench
open Mec.Permutation

module Parameters = struct
  let width = 3

  let full_rounds = 8

  let partial_rounds = 56

  let round_constants = Ark_poseidon128.v

  let partial_round_idx_to_permute = 2

  let linear_transformation = Mds_poseidon128.v

  (* This is the first alpha such that pgc(alpha, p - 1) = 1 *)
  let alpha = Z.of_string "5"

  let batch_size = 3
end

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

module Naive = Hades.Make (Parameters) (Scalar)
module Optimised = HadesLinearOptimisation.Make (Parameters) (Scalar)

let n = 3

let inputs = Array.init n ~f:(fun _i -> Scalar.random ())

let t1 =
  let name =
    Printf.sprintf
      "Benchmark one permutation of naive implementation of HADES with \
       ocaml-ff backend on an input of %d elements"
      n
  in
  Bench.Test.create ~name (fun () ->
      let ctxt = Naive.init inputs in
      let () = Naive.apply ctxt in
      let _v = Naive.get ctxt in
      ())

let t2 =
  let name =
    Printf.sprintf
      "Benchmark one permutation of optimised implementation of HADES with \
       ocaml-ff backend on an input of %d elements"
      n
  in
  Bench.Test.create ~name (fun () ->
      let ctxt = Optimised.init inputs in
      let () = Optimised.apply ctxt in
      let _v = Optimised.get ctxt in
      ())

let command = Bench.make_command [t1; t2]

let () = Core.Command.run command
