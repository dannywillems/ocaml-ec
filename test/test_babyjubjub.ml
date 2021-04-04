module ValueGeneration =
  Ec_pbt.MakeValueGeneration (Ec_babyjubjub.TwistedEdwards)
module Equality = Ec_pbt.MakeEquality (Ec_babyjubjub.TwistedEdwards)
module Properties = Ec_pbt.MakeECProperties (Ec_babyjubjub.TwistedEdwards)

let test_addition_vectors =
  [ ( ( "17777552123799933955779906779655732241715742912184938656739573121738514868268",
        "2626589144620713026669568689430873010625803728049924121243784502389097019475"
      ),
      ( "16540640123574156134436876038791482806971768689494387082833631921987005038935",
        "20819045374670962167435360035096875258406992893633759881276124905556507972311"
      ),
      ( "7916061937171219682591368294088513039687205273691143098332585753343424131937",
        "14035240266687799601661095864649209771790948434046947201833777492504781204499"
      ) ) ]

let test_addition () =
  List.iter
    (fun ((x1, y1), (x2, y2), (x3, y3)) ->
      let x1 = Ec_babyjubjub.TwistedEdwards.BaseField.of_string x1 in
      let y1 = Ec_babyjubjub.TwistedEdwards.BaseField.of_string y1 in
      let x2 = Ec_babyjubjub.TwistedEdwards.BaseField.of_string x2 in
      let y2 = Ec_babyjubjub.TwistedEdwards.BaseField.of_string y2 in
      let x3 = Ec_babyjubjub.TwistedEdwards.BaseField.of_string x3 in
      let y3 = Ec_babyjubjub.TwistedEdwards.BaseField.of_string y3 in
      let p1 = Ec_babyjubjub.TwistedEdwards.from_coordinates_exn ~u:x1 ~v:y1 in
      let p2 = Ec_babyjubjub.TwistedEdwards.from_coordinates_exn ~u:x2 ~v:y2 in
      let p3 = Ec_babyjubjub.TwistedEdwards.from_coordinates_exn ~u:x3 ~v:y3 in
      assert (Ec_babyjubjub.TwistedEdwards.(eq (add p1 p2) p3)) ;
      assert (Ec_babyjubjub.TwistedEdwards.(eq (add p2 p1) p3)))
    test_addition_vectors

let () =
  let open Alcotest in
  run
    ~verbose:true
    "BabyJubjub"
    [ ("Vectors", [Alcotest.test_case "test vectors" `Quick test_addition]);
      ValueGeneration.get_tests ();
      Properties.get_tests ();
      Equality.get_tests () ]
