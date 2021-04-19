module ValueGeneration = Ec_pbt.MakeValueGeneration (Ec_babyjubjub.Affine)
module Equality = Ec_pbt.MakeEquality (Ec_babyjubjub.Affine)
module Properties = Ec_pbt.MakeECProperties (Ec_babyjubjub.Affine)
module EdwardsCurveProperties =
  Ec_pbt.MakeEdwardsCurveProperties (Ec_babyjubjub.Affine)

let test_doubling () =
  let vectors =
    [ ( ( "17777552123799933955779906779655732241715742912184938656739573121738514868268",
          "2626589144620713026669568689430873010625803728049924121243784502389097019475"
        ),
        ( "6890855772600357754907169075114257697580319025794532037257385534741338397365",
          "4338620300185947561074059802482547481416142213883829469920100239455078257889"
        ) ) ]
  in
  List.iter
    (fun ((x1, y1), (x2, y2)) ->
      let x1 = Ec_babyjubjub.Affine.BaseField.of_string x1 in
      let y1 = Ec_babyjubjub.Affine.BaseField.of_string y1 in
      let x2 = Ec_babyjubjub.Affine.BaseField.of_string x2 in
      let y2 = Ec_babyjubjub.Affine.BaseField.of_string y2 in
      let p1 = Ec_babyjubjub.Affine.from_coordinates_exn ~u:x1 ~v:y1 in
      let p2 = Ec_babyjubjub.Affine.from_coordinates_exn ~u:x2 ~v:y2 in
      assert (Ec_babyjubjub.Affine.(eq (double p1) p2)))
    vectors

let test_addition () =
  let vectors =
    [ ( ( "17777552123799933955779906779655732241715742912184938656739573121738514868268",
          "2626589144620713026669568689430873010625803728049924121243784502389097019475"
        ),
        ( "16540640123574156134436876038791482806971768689494387082833631921987005038935",
          "20819045374670962167435360035096875258406992893633759881276124905556507972311"
        ),
        ( "7916061937171219682591368294088513039687205273691143098332585753343424131937",
          "14035240266687799601661095864649209771790948434046947201833777492504781204499"
        ) ) ]
  in
  List.iter
    (fun ((x1, y1), (x2, y2), (x3, y3)) ->
      let x1 = Ec_babyjubjub.Affine.BaseField.of_string x1 in
      let y1 = Ec_babyjubjub.Affine.BaseField.of_string y1 in
      let x2 = Ec_babyjubjub.Affine.BaseField.of_string x2 in
      let y2 = Ec_babyjubjub.Affine.BaseField.of_string y2 in
      let x3 = Ec_babyjubjub.Affine.BaseField.of_string x3 in
      let y3 = Ec_babyjubjub.Affine.BaseField.of_string y3 in
      let p1 = Ec_babyjubjub.Affine.from_coordinates_exn ~u:x1 ~v:y1 in
      let p2 = Ec_babyjubjub.Affine.from_coordinates_exn ~u:x2 ~v:y2 in
      let p3 = Ec_babyjubjub.Affine.from_coordinates_exn ~u:x3 ~v:y3 in
      assert (Ec_babyjubjub.Affine.(eq (add p1 p2) p3)) ;
      assert (Ec_babyjubjub.Affine.(eq (add p2 p1) p3)) ;
      assert (Ec_babyjubjub.Affine.(not (eq (add p1 p2) p1))))
    vectors

let test_mul_scalar () =
  let vectors =
    [ ( ( "17777552123799933955779906779655732241715742912184938656739573121738514868268",
          "2626589144620713026669568689430873010625803728049924121243784502389097019475"
        ),
        "14035240266687799601661095864649209771790948434046947201833777492504781204499",
        ( "17070357974431721403481313912716834497662307308519659060910483826664480189605",
          "4014745322800118607127020275658861516666525056516280575712425373174125159339"
        ) ) ]
  in
  List.iter
    (fun ((x1, y1), n, (x2, y2)) ->
      let x1 = Ec_babyjubjub.Affine.BaseField.of_string x1 in
      let y1 = Ec_babyjubjub.Affine.BaseField.of_string y1 in
      let x2 = Ec_babyjubjub.Affine.BaseField.of_string x2 in
      let y2 = Ec_babyjubjub.Affine.BaseField.of_string y2 in
      let n = Ec_babyjubjub.Affine.ScalarField.of_string n in
      let p1 = Ec_babyjubjub.Affine.from_coordinates_exn ~u:x1 ~v:y1 in
      let p2 = Ec_babyjubjub.Affine.from_coordinates_exn ~u:x2 ~v:y2 in
      assert (Ec_babyjubjub.Affine.(eq (mul p1 n) p2)))
    vectors

let test_random_is_not_small_order () =
  assert (not Ec_babyjubjub.Affine.(is_small_order (random ())))

let test_random_points_not_on_curve () =
  (* pick random values u and v and test constructors fail *)
  let u = Ec_babyjubjub.Affine.BaseField.random () in
  let v = Ec_babyjubjub.Affine.BaseField.random () in
  let bytes =
    Bytes.concat
      Bytes.empty
      [ Ec_babyjubjub.Affine.BaseField.to_bytes u;
        Ec_babyjubjub.Affine.BaseField.to_bytes v ]
  in
  (* check_bytes *)
  assert (not (Ec_babyjubjub.Affine.check_bytes bytes)) ;
  (* of_bytes_opt *)
  assert (Option.is_none (Ec_babyjubjub.Affine.of_bytes_opt bytes)) ;
  (* of_bytes_exn *)
  ( try
      ignore (Ec_babyjubjub.Affine.of_bytes_exn bytes) ;
      assert false
    with
  | Ec_babyjubjub.Affine.Not_on_curve _ -> ()
  | _ -> assert false ) ;
  (* from_coordinates_opt *)
  assert (Option.is_none (Ec_babyjubjub.Affine.from_coordinates_opt ~u ~v)) ;
  (* from_coordinates_exn *)
  try
    ignore (Ec_babyjubjub.Affine.from_coordinates_exn ~u ~v) ;
    assert false
  with
  | Ec_babyjubjub.Affine.Not_on_curve _ -> ()
  | _ -> assert false

let () =
  let open Alcotest in
  run
    ~verbose:true
    "BabyJubjub"
    [ ( "Vectors",
        [ Alcotest.test_case "test vectors addition" `Quick test_addition;
          Alcotest.test_case "test scalar multiplication" `Quick test_mul_scalar;
          Alcotest.test_case
            "test random elements are in the prime subgroup"
            `Quick
            (Ec_pbt.repeat 100 test_random_is_not_small_order);
          Alcotest.test_case
            "test random coordinates u, v do not give a point on the curve"
            `Quick
            (Ec_pbt.repeat 100 test_random_points_not_on_curve);
          Alcotest.test_case "test vectors doubling" `Quick test_doubling ] );
      ValueGeneration.get_tests ();
      Properties.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Equality.get_tests () ]
