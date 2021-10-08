open Core
open Core_bench
open Mec.Hash

let t1 =
  let module Scalar = Ff.MakeFp (struct
    let prime_order =
      Z.of_string
        "52435875175126190479447740508185965837690552500527637822603658699938581184513"
  end) in
  let module Poseidon = Neptunus.Make (Scalar) in
  let n = 3 in
  let inputs = Array.init n ~f:(fun _i -> Scalar.random ()) in
  let name =
    Printf.sprintf
      "Benchmark one permutation of Neptunus with ocaml-ff on an input of %d \
       elements"
      n
  in
  Bench.Test.create ~name (fun () ->
      let ctxt = Poseidon.Strategy.init inputs in
      let () = Poseidon.Strategy.apply_perm ctxt in
      let _v = (Poseidon.Strategy.get ctxt).(0) in
      ())

let t2 =
  let module Scalar = Bls12_381.Fr in
  let module Poseidon = Neptunus.Make (Scalar) in
  let n = 3 in
  let inputs = Array.init n ~f:(fun _i -> Scalar.random ()) in
  let name =
    Printf.sprintf
      "Benchmark one permutation of Poseidon252 with blst backend on an input \
       of %d elements"
      n
  in
  Bench.Test.create ~name (fun () ->
      let ctxt = Poseidon.Strategy.init inputs in
      let () = Poseidon.Strategy.apply_perm ctxt in
      let _v = Poseidon.Strategy.get ctxt in
      ())

let t3 =
  let module Poseidon = Neptunus.MakeInplace (Bls12_381.Fr) in
  let n = 3 in
  let inputs = Array.init n ~f:(fun _i -> Bls12_381.Fr.random ()) in
  let name =
    Printf.sprintf
      "Benchmark one permutation of Poseidon252 with blst backend and using \
       inplace operations on an input of %d elements"
      n
  in
  Bench.Test.create ~name (fun () ->
      let ctxt = Poseidon.Strategy.init inputs in
      let () = Poseidon.Strategy.apply_perm ctxt in
      let _v = Poseidon.Strategy.get ctxt in
      ())

let command = Bench.make_command [t1; t2; t3]

let () = Core.Command.run command
