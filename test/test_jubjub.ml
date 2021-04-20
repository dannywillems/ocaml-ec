module ValueGeneration = Ec_pbt.MakeValueGeneration (Ec_jubjub.Affine)
module Equality = Ec_pbt.MakeEquality (Ec_jubjub.Affine)
module Properties = Ec_pbt.MakeECProperties (Ec_jubjub.Affine)
module EdwardsCurveProperties =
  Ec_pbt.MakeEdwardsCurveProperties (Ec_jubjub.Affine)

let test_vectors () =
  (* Coming from
     https://github.com/zcash/librustzcash/blob/1b4aab0b76d465e4fe548f230b7c3ebdc398a1a5/zcash_primitives/src/constants.rs *)
  let points =
    [ ( "0x73c016a42ded9578b5ea25de7ec0e3782f0c718f6f0fbadd194e42926f661b51",
        "0x289e87a2d3521b5779c9166b837edc5ef9472e8bc04e463277bfabd432243cca" );
      ( "0x15a36d1f0f390d8852a35a8c1908dd87a361ee3fd48fdf77b9819dc82d90607e",
        "0x015d8c7f5b43fe33f7891142c001d9251f3abeeb98fad3e87b0dc53c4ebf1891" );
      ( "0x664321a58246e2f6eb69ae39f5c84210bae8e5c46641ae5c76d6f7c2b67fc475",
        "0x362e1500d24eee9ee000a46c8e8ce8538bb22a7f1784b49880ed502c9793d457" );
      ( "0x323a6548ce9d9876edc5f4a9cff29fd57d02d50e654b87f24c767804c1c4a2cc",
        "0x2f7ee40c4b56cad891070acbd8d947b75103afa1a11f6a8584714beca33570e9" );
      ( "0x3bd2666000b5479689b64b4e03362796efd5931305f2f0bf46809430657f82d1",
        "0x494bc52103ab9d0a397832381406c9e5b3b9d8095859d14c99968299c3658aef" );
      ( "0x63447b2ba31bb28ada049746d76d3ee51d9e5ca21135ff6fcb3c023258d32079",
        "0x64ec4689e8bfb6e564cdb1070a136a28a80200d2c66b13a7436082119f8d629a" )
    ]
  in
  List.iter
    (fun (u, v) ->
      (* from_coordinates_exn *)
      ignore
      @@ Ec_jubjub.Affine.from_coordinates_exn
           ~u:(Ec_jubjub.Affine.BaseField.of_string u)
           ~v:(Ec_jubjub.Affine.BaseField.of_string v) ;
      (* from_coordinates_opt *)
      assert (
        Option.is_some
          (Ec_jubjub.Affine.from_coordinates_opt
             ~u:(Ec_jubjub.Affine.BaseField.of_string u)
             ~v:(Ec_jubjub.Affine.BaseField.of_string v)) ) ;
      (* convert to bytes. Use Zarith for simplicity as points are given in hexa *)
      let bytes =
        Bytes.concat
          Bytes.empty
          [ (Bytes.of_string @@ Z.(to_bits (of_string u)));
            (Bytes.of_string @@ Z.(to_bits (of_string v))) ]
      in
      (* of_bytes_opt *)
      assert (Option.is_some (Ec_jubjub.Affine.of_bytes_opt bytes)) ;
      (* of_bytes_exn *)
      ignore @@ Ec_jubjub.Affine.of_bytes_exn bytes ;
      (* check_bytes *)
      assert (Ec_jubjub.Affine.check_bytes bytes))
    points

let test_random_is_not_small_order () =
  assert (not Ec_jubjub.Affine.(is_small_order (random ())))

let test_random_points_not_on_curve () =
  (* pick random values u and v and test constructors fail *)
  let u = Ec_jubjub.Affine.BaseField.random () in
  let v = Ec_jubjub.Affine.BaseField.random () in
  let bytes =
    Bytes.concat
      Bytes.empty
      [ Ec_jubjub.Affine.BaseField.to_bytes u;
        Ec_jubjub.Affine.BaseField.to_bytes v ]
  in
  (* check_bytes *)
  assert (not (Ec_jubjub.Affine.check_bytes bytes)) ;
  (* of_bytes_opt *)
  assert (Option.is_none (Ec_jubjub.Affine.of_bytes_opt bytes)) ;
  (* of_bytes_exn *)
  ( try
      ignore (Ec_jubjub.Affine.of_bytes_exn bytes) ;
      assert false
    with
  | Ec_jubjub.Affine.Not_on_curve _ -> ()
  | _ -> assert false ) ;
  (* from_coordinates_opt *)
  assert (Option.is_none (Ec_jubjub.Affine.from_coordinates_opt ~u ~v)) ;
  (* from_coordinates_exn *)
  try
    ignore (Ec_jubjub.Affine.from_coordinates_exn ~u ~v) ;
    assert false
  with
  | Ec_jubjub.Affine.Not_on_curve _ -> ()
  | _ -> assert false

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Jubjub"
    [ ( "Vectors",
        [ Alcotest.test_case
            "test random elements are in the prime subgroup"
            `Quick
            (Ec_pbt.repeat 1000 test_random_is_not_small_order);
          Alcotest.test_case "test vectors elements" `Quick test_vectors ] );
      ( "Tests random",
        [ Alcotest.test_case
            "test random coordinates do not give a point on the curve"
            `Quick
            (Ec_pbt.repeat 100 test_random_points_not_on_curve) ] );
      ValueGeneration.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Properties.get_tests ();
      Equality.get_tests () ]
