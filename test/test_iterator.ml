let test_one_byte () =
  (* Single byte *)
  let test_vectors =
    [ ("\x00", [0; 0; 0; 0; 0; 0; 0; 0]);
      ("\x0f", [1; 1; 1; 1; 0; 0; 0; 0]);
      ("\x10", [0; 0; 0; 0; 1; 0; 0; 0]) ]
  in
  List.iter
    (fun (b, expected_l) ->
      let iterator =
        Pedersen_hash.Iterator.Bit.of_bytes_le (Bytes.of_string b)
      in
      List.iter
        (fun exp_b ->
          assert (Pedersen_hash.Iterator.Bit.next iterator = Some exp_b))
        expected_l ;
      assert (Pedersen_hash.Iterator.Bit.next iterator = None))
    test_vectors

let test_from_bool_list () =
  (* Single byte *)
  let test_vectors =
    [ ([true; true; true], [1; 1; 1]);
      ([false; true; true], [0; 1; 1]);
      ([true; true; false; true; false], [1; 1; 0; 1; 0]) ]
  in
  List.iter
    (fun (b, expected_l) ->
      let iterator = Pedersen_hash.Iterator.Bit.create_from_bool_list b in
      List.iter
        (fun exp_b ->
          assert (Pedersen_hash.Iterator.Bit.next iterator = Some exp_b))
        expected_l ;
      assert (Pedersen_hash.Iterator.Bit.next iterator = None))
    test_vectors

let test_multiple_bytes () =
  (* The individual bytes are given in big endian and are concatenated in the
     order of appearance in the string
     \x10\x00 represents 0x0010 in big endian, i.e. 16.
  *)
  let test_vectors =
    [ ("\x00\x00", [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]);
      ("\x0f\x00", [1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]);
      ("\x10\x00", [0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]);
      ( "\x10\x00\x10",
        [0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0]
      );
      ( "\x10\x00\x10\x2f",
        [ (* 0x10 *)
          0;
          0;
          0;
          0;
          1;
          0;
          0;
          0;
          (* 0x00 *)
          0;
          0;
          0;
          0;
          0;
          0;
          0;
          0;
          (* 0x10 *)
          0;
          0;
          0;
          0;
          1;
          0;
          0;
          0;
          (* 0x2f *)
          1;
          1;
          1;
          1;
          0;
          1;
          0;
          0 ] ) ]
  in
  List.iter
    (fun (b, expected_l) ->
      let iterator =
        Pedersen_hash.Iterator.Bit.of_bytes_le (Bytes.of_string b)
      in
      List.iter
        (fun exp_b ->
          assert (Pedersen_hash.Iterator.Bit.next iterator = Some exp_b))
        expected_l ;
      assert (Pedersen_hash.Iterator.Bit.next iterator = None))
    test_vectors

let () =
  Alcotest.run
    ~verbose:true
    "Iterator"
    [ ( "Bytes",
        [ Alcotest.test_case "One byte" `Quick test_one_byte;
          Alcotest.test_case "Multiple bytes" `Quick test_multiple_bytes ] );
      ( "Bool list",
        [Alcotest.test_case "Test vectors" `Quick test_from_bool_list] ) ]
