open Mec.Hash

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.(
      ((one + one) ** 254)
      + Z.of_string "45560315531419706090280762371685220353")
end)

module Poseidon = Poseidon128.Make (Scalar)

let test_perm_is_consistent () =
  let x = Array.make Poseidon128.Constant.width (Scalar.of_string "17") in
  let y = Array.make Poseidon128.Constant.width (Scalar.of_string "17") in
  let z = Array.make Poseidon128.Constant.width (Scalar.of_string "19") in

  let state_x = Poseidon.Strategy.init x in
  let state_y = Poseidon.Strategy.init y in
  let state_z = Poseidon.Strategy.init z in

  Poseidon.Strategy.apply_perm state_x ;
  Poseidon.Strategy.apply_perm state_y ;
  Poseidon.Strategy.apply_perm state_z ;

  let res_x = Poseidon.Strategy.get state_x in
  let res_y = Poseidon.Strategy.get state_y in
  let res_z = Poseidon.Strategy.get state_z in
  assert (Array.for_all2 Scalar.eq res_x res_y) ;
  assert (not @@ Array.for_all2 Scalar.eq res_x res_z)

let test_vectors_hades_orchard () =
  let vectors =
    [ ( [| Scalar.zero; Scalar.one; Scalar.(one + one) |],
        [| "11594746544082808193684844477463633150342086886343960773487848974120850181264";
           "8014665628031024095124899320855562151905643011245776438370704956349329180958";
           "7719389013904241493524551054574463394884187129046035658329571349046169099816"
        |] ) ]
  in
  List.iter
    (fun (input, expected_output) ->
      let s = Poseidon.Strategy.init input in
      Poseidon.Strategy.apply_perm s ;
      let res = Poseidon.Strategy.get s in
      let expected_output =
        Array.map (fun s -> Scalar.of_string s) expected_output
      in
      if
        not
          (List.for_all2
             Scalar.eq
             (Array.to_list expected_output)
             (Array.to_list res))
      then
        let res =
          String.concat
            "; "
            (Array.to_list @@ Array.map (fun s -> Scalar.to_string s) res)
        in
        let expected_output =
          String.concat
            "; "
            ( Array.to_list
            @@ Array.map (fun s -> Scalar.to_string s) expected_output )
        in
        Alcotest.failf
          "Computed result: [%s]. Expected result: [%s]\n"
          res
          expected_output)
    vectors

let test_vectors_poseidon_orchard () =
  let open Poseidon in
  let test_inputs =
    [ ( [| "0"; "1"; "36893488147419103232" |],
        "9294224303572826231334390170707418973776412638020053350998514035066722916288"
      ) ]
  in
  List.iter
    (fun (inputs, expected_output) ->
      let inputs = Array.map (fun x -> Scalar.of_string x) inputs in
      let ctxt = Hash.init () in
      let ctxt = Hash.digest ctxt inputs in
      let v = Hash.get ctxt in
      let exp_res = Scalar.of_string expected_output in
      assert (Scalar.eq v exp_res))
    test_inputs

let () =
  Alcotest.run
    ~verbose:true
    "Poseidon128"
    [ ( "Properties",
        [Alcotest.test_case "Perm is consistent" `Quick test_perm_is_consistent]
      );
      ( "Test vectors for Hades Poseidon128",
        [ Alcotest.test_case
            "Test vectors from zcash-hackworks/zcash-test-vectors"
            `Quick
            test_vectors_hades_orchard ] )
      (* ( "Test vectors for Poseidon Poseidon128",
       *   [ Alcotest.test_case
       *       "Test vectors from zcash-hackworks/zcash-test-vectors"
       *       `Quick
       *       test_vectors_poseidon_orchard ] ) *) ]
