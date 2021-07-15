module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.(
      ((one + one) ** 254)
      + Z.of_string "45560315531419706090280762371685220353")
end)

open Poseidon
module Poseidon = Orchard.Make (Scalar)

let test_perm_is_consistent () =
  let x = Array.make Orchard.Constant.width (Scalar.of_string "17") in
  let y = Array.make Orchard.Constant.width (Scalar.of_string "17") in
  let z = Array.make Orchard.Constant.width (Scalar.of_string "19") in

  let state_x = Poseidon.Strategy.init x in
  let state_y = Poseidon.Strategy.init y in
  let state_z = Poseidon.Strategy.init z in

  Poseidon.Strategy.apply_perm state_x ;
  Poseidon.Strategy.apply_perm state_y ;
  Poseidon.Strategy.apply_perm state_z ;

  assert (Poseidon.Strategy.(get state_x = get state_y)) ;
  assert (Poseidon.Strategy.(get state_x <> get state_z))

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

let () =
  Alcotest.run
    ~verbose:true
    "Poseidon Orchard"
    [ ( "Properties",
        [Alcotest.test_case "Perm is consistent" `Quick test_perm_is_consistent]
      );
      ( "Test vectors for Hades Orchard",
        [ Alcotest.test_case
            "Test vectors from zcash-hackworks/zcash-test-vectors"
            `Quick
            test_vectors_hades_orchard ] )
      (* ( "Test vectors for Poseidon252",
       *   [ Alcotest.test_case
       *       "Test vectors from dusk-network/poseidon252"
       *       `Quick
       *       test_vectors_poseidon252 ] ) *) ]
