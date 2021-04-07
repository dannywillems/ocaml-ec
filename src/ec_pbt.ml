let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f )

module MakeEquality (G : Ec_sig.BASE) = struct
  (** Verify the equality is correct with the value zero *)
  let zero () = assert (G.eq G.zero G.zero)

  (** Verify the equality is correct with the value one *)
  let one () = assert (G.eq G.one G.one)

  (** Verify the equality of two random values created invidually *)
  let random_same_objects () =
    let random = G.random () in
    assert (G.eq random random)

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "equality",
      [ test_case "zero" `Quick (repeat 1 zero);
        test_case "one" `Quick (repeat 1 one);
        test_case "random_same_objects" `Quick (repeat 100 random_same_objects)
      ] )
end

module MakeValueGeneration (G : Ec_sig.BASE) = struct
  let random () = ignore @@ G.random ()

  let negation_with_random () =
    let random = G.random () in
    ignore @@ G.negate random

  let negation_with_zero () = ignore @@ G.negate G.zero

  let negation_with_one () = ignore @@ G.negate G.one

  let double_with_zero () = ignore @@ G.double G.zero

  let double_with_one () = ignore @@ G.double G.one

  let double_with_random () =
    let g = G.random () in
    ignore @@ G.double g

  let addition_generates_valid_point () =
    assert (G.(check_bytes (to_bytes (add (random ()) (random ())))))

  let double_generates_valid_point () =
    assert (G.(check_bytes (to_bytes (double (random ())))))

  let scalar_multiplication_generates_valid_point () =
    assert (G.(check_bytes (to_bytes (mul (random ()) (ScalarField.random ())))))

  let check_bytes_random_with_to_bytes () =
    let g = G.random () in
    assert (G.(check_bytes (to_bytes g)))

  let negate_generates_a_valid_point () =
    let g = G.random () in
    assert (G.(check_bytes (to_bytes (negate g))))

  let of_bytes_with_to_bytes_are_inverse_functions () =
    let g = G.random () in
    assert (G.(eq (of_bytes_exn (to_bytes g)) g))

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "value generation",
      [ test_case "random" `Quick (repeat 100 random);
        test_case "negate_with_one" `Quick (repeat 1 negation_with_one);
        test_case "negate_with_zero" `Quick (repeat 1 negation_with_zero);
        test_case "negate_with_random" `Quick (repeat 100 negation_with_random);
        test_case "double_with_random" `Quick (repeat 100 double_with_random);
        test_case
          "negate generates a valid point"
          `Quick
          (repeat 100 negate_generates_a_valid_point);
        test_case
          "addition generates a valid point"
          `Quick
          (repeat 100 addition_generates_valid_point);
        test_case
          "double generates a valid point"
          `Quick
          (repeat 100 double_generates_valid_point);
        test_case
          "scalar multiplication generates a valid point"
          `Quick
          (repeat 100 scalar_multiplication_generates_valid_point);
        test_case
          "of_bytes_exn and to_bytes are inverse functions"
          `Quick
          (repeat 100 of_bytes_with_to_bytes_are_inverse_functions);
        test_case
          "check bytes on random with to_bytes"
          `Quick
          (repeat 100 check_bytes_random_with_to_bytes);
        test_case "double_with_one" `Quick (repeat 1 double_with_one);
        test_case "double_with_zero" `Quick (repeat 100 double_with_zero) ] )
end

module MakeIsZero (G : Ec_sig.BASE) = struct
  let with_zero_value () = assert (G.is_zero G.zero = true)

  let with_one_value () = assert (G.is_zero G.one = false)

  let with_random_value () = assert (G.is_zero (G.random ()) = false)

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "is_zero",
      [ test_case "with zero value" `Quick (repeat 1 with_zero_value);
        test_case "with one value" `Quick (repeat 1 with_one_value);
        test_case "with random value" `Quick (repeat 100 with_random_value) ] )
end

module MakeECProperties (G : Ec_sig.BASE) = struct
  (** Verify that a random point is valid *)
  let check_bytes_random () = assert (G.(check_bytes @@ to_bytes @@ random ()))

  (** Verify that the zero point is valid *)
  let check_bytes_zero () = assert (G.(check_bytes @@ to_bytes @@ zero))

  (** Verify that the fixed generator point is valid *)
  let check_bytes_one () = assert (G.(check_bytes @@ to_bytes @@ one))

  (** Verify that doubling a random point gives a valid point *)
  let check_bytes_random_double () =
    assert (G.(check_bytes @@ to_bytes @@ double (random ())))

  (** Verify that the sum of random points is valid *)
  let check_bytes_random_sum () =
    assert (G.(check_bytes @@ to_bytes @@ add (random ()) (random ())))

  (** Verify that multiplying a random point by a scalar gives a valid point *)
  let check_bytes_random_multiplication () =
    assert (
      G.(check_bytes @@ to_bytes @@ mul (random ()) (ScalarField.random ())) )

  (** Verify 0_S * g_EC = 0_EC where 0_S is the zero of the scalar field, 0_EC
  is the point at infinity and g_EC is an element of the EC *)
  let zero_scalar_nullifier_random () =
    let random = G.random () in
    assert (G.is_zero (G.mul random G.ScalarField.zero))

  (** Verify 0_S * 0_EC = 0_EC where 0_S is the zero of the scalar field and
  0_EC is the point at infinity of the EC *)
  let zero_scalar_nullifier_zero () =
    assert (G.is_zero (G.mul G.zero G.ScalarField.zero))

  (** Verify 0_S * 1_EC = 0_EC where 0_S is the 0 of the scalar field, 1_EC is a
  fixed generator and 0_EC is the point at infinity of the EC *)
  let zero_scalar_nullifier_one () =
    assert (G.is_zero (G.mul G.one G.ScalarField.zero))

  let multiply_by_one_does_nothing () =
    let g = G.random () in
    assert (G.(eq (mul g ScalarField.one) g))

  (** Verify -(-g) = g where g is an element of the EC *)
  let opposite_of_opposite () =
    let random = G.random () in
    assert (G.eq (G.negate (G.negate random)) random)

  let opposite_of_opposite_using_scalar () =
    let r = G.random () in
    assert (
      G.(eq r (mul r (ScalarField.negate (ScalarField.negate ScalarField.one))))
    )

  (** Verify -(-0_EC) = 0_EC where 0_EC is the point at infinity of the EC *)
  let opposite_of_zero_is_zero () = assert (G.eq (G.negate G.zero) G.zero)

  (** Verify -(-0_EC) = 0_EC where 0_EC is the point at infinity of the EC *)
  let opposite_of_opposite_of_zero_is_zero () =
    assert (G.eq (G.negate (G.negate G.zero)) G.zero)

  (** Verify -(-0_EC) = 0_EC where 0_EC is the point at infinity of the EC *)
  let opposite_of_opposite_of_one_is_one () =
    assert (G.eq (G.negate (G.negate G.one)) G.one)

  (** Verify g1 + (g2 + g3) = (g1 + g2) + g3 *)
  let additive_associativity () =
    let g1 = G.random () in
    let g2 = G.random () in
    let g3 = G.random () in
    assert (G.eq (G.add (G.add g1 g2) g3) (G.add g1 (G.add g2 g3)))

  (** Verify (g1 + g2) = (g2 + g1) *)
  let additive_commutativity () =
    let g1 = G.random () in
    let g2 = G.random () in
    assert (G.eq (G.add g1 g2) (G.add g2 g1))

  (** Verify that g + (-g) = 0 *)
  let opposite_existential_property () =
    let g = G.random () in
    assert (G.(eq (add g (negate g)) zero))

  (** Verify a (g1 + g2) = a * g1 + a * g2 where a is a scalar, g1, g2 two
  elements of the EC *)
  let distributivity () =
    let s = G.ScalarField.random () in
    let g1 = G.random () in
    let g2 = G.random () in
    assert (G.eq (G.mul (G.add g1 g2) s) (G.add (G.mul g1 s) (G.mul g2 s)))

  (** Verify (a + -a) * g = a * g - a * g = 0 *)
  let opposite_equality () =
    let a = G.ScalarField.random () in
    let g = G.random () in
    assert (G.(eq (mul g (ScalarField.add a (ScalarField.negate a))) zero)) ;
    assert (G.(eq zero (add (mul g a) (mul g (ScalarField.negate a))))) ;
    assert (
      G.(
        eq
          (mul g (ScalarField.add a (ScalarField.negate a)))
          (add (mul g a) (mul g (ScalarField.negate a)))) )

  (** a g + b + g = (a + b) g*)
  let additive_associativity_with_scalar () =
    let a = G.ScalarField.random () in
    let b = G.ScalarField.random () in
    let g = G.random () in
    let left = G.(add (mul g a) (mul g b)) in
    let right = G.(mul g (ScalarField.add a b)) in
    assert (G.(eq left right))

  (** (a * b) g = a (b g) = b (a g) *)
  let multiplication_properties_on_base_field_element () =
    let a = G.ScalarField.random () in
    let b = G.ScalarField.random () in
    let g = G.random () in
    assert (G.(eq (mul g (ScalarField.mul a b)) (mul (mul g a) b))) ;
    assert (G.(eq (mul g (ScalarField.mul a b)) (mul (mul g b) a)))

  (** Verify (-s) * g = s * (-g) *)
  let opposite_of_scalar_is_opposite_of_ec () =
    let s = G.ScalarField.random () in
    let g = G.random () in
    let left = G.mul g (G.ScalarField.negate s) in
    let right = G.mul (G.negate g) s in
    assert (G.eq left right)

  let generator_is_of_prime_order () =
    assert (G.(eq (mul one (G.ScalarField.of_z G.ScalarField.order)) zero))

  let mul_by_order_of_scalar_field_equals_zero () =
    let s = G.ScalarField.random () in
    let g = G.random () in
    assert (G.(eq (mul (mul g s) (G.ScalarField.of_z G.ScalarField.order)) zero)) ;
    assert (
      G.(eq (mul (mul one s) (G.ScalarField.of_z G.ScalarField.order)) zero) )

  (** Verify 2*g = g + g *)
  let double () =
    let s = G.random () in
    assert (G.(eq (double s) (add s s)))

  let inverse_on_scalar () =
    let g = G.random () in
    let a = G.ScalarField.random () in
    let inv_a = G.ScalarField.inverse_exn a in
    let ga = G.mul g a in
    let ga_inv = G.mul g inv_a in
    let res1 = G.mul g (G.ScalarField.mul inv_a a) in
    let res2 = G.mul ga_inv a in
    let res3 = G.mul ga inv_a in
    assert (G.(eq res2 res3)) ;
    (* g * (a * a^(-1)) = g *)
    assert (G.(eq res1 g)) ;
    (* (g * a^(-1)) * a = g *)
    assert (G.(eq res2 g)) ;
    (* (g * a) * a^(-1) = g *)
    assert (G.(eq res3 g))

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "Group properties",
      [ test_case "check_bytes_random" `Quick (repeat 100 check_bytes_random);
        test_case "check_bytes_zero" `Quick (repeat 1 check_bytes_zero);
        test_case "check_bytes_one" `Quick (repeat 1 check_bytes_one);
        test_case
          "check_bytes_random_double"
          `Quick
          (repeat 100 check_bytes_random_double);
        test_case
          "check_bytes_random_sum"
          `Quick
          (repeat 100 check_bytes_random_sum);
        test_case
          "check_bytes_random_multiplication"
          `Quick
          (repeat 100 check_bytes_random_multiplication);
        test_case
          "zero_scalar_nullifier_one"
          `Quick
          (repeat 1 zero_scalar_nullifier_one);
        test_case
          "zero_scalar_nullifier_zero"
          `Quick
          (repeat 1 zero_scalar_nullifier_zero);
        test_case
          "zero_scalar_nullifier_random"
          `Quick
          (repeat 100 zero_scalar_nullifier_random);
        test_case
          "multiply_by_one_does_nothing"
          `Quick
          (repeat 100 multiply_by_one_does_nothing);
        test_case
          "opposite_of_opposite"
          `Quick
          (repeat 100 opposite_of_opposite);
        test_case
          "opposite_of_opposite_using_scalar"
          `Quick
          (repeat 100 opposite_of_opposite_using_scalar);
        test_case
          "opposite_of_zero_is_zero"
          `Quick
          (repeat 1 opposite_of_zero_is_zero);
        test_case
          "opposite_of_opposite_of_zero_is_zero"
          `Quick
          (repeat 1 opposite_of_opposite_of_zero_is_zero);
        test_case
          "opposite_of_opposite_of_one_is_one"
          `Quick
          (repeat 1 opposite_of_opposite_of_one_is_one);
        test_case "opposite_equality" `Quick (repeat 1 opposite_equality);
        test_case "distributivity" `Quick (repeat 100 distributivity);
        test_case
          "opposite_of_scalar_is_opposite_of_ec"
          `Quick
          (repeat 100 opposite_of_scalar_is_opposite_of_ec);
        test_case
          "opposite_existential_property"
          `Quick
          (repeat 100 opposite_existential_property);
        test_case
          "mul_by_order_of_base_field_equals_element"
          `Quick
          (repeat 100 mul_by_order_of_scalar_field_equals_zero);
        test_case
          "multiplication_properties_on_base_field_element"
          `Quick
          (repeat 100 multiplication_properties_on_base_field_element);
        test_case "double" `Quick (repeat 100 double);
        test_case
          "additive_associativity_with_scalar"
          `Quick
          (repeat 100 additive_associativity_with_scalar);
        test_case "inverse on scalar" `Quick (repeat 100 inverse_on_scalar);
        test_case
          "additive_associativity"
          `Quick
          (repeat 100 additive_associativity);
        test_case
          "additive_commutativity"
          `Quick
          (repeat 100 additive_commutativity);
        test_case
          "Generator is of prime order"
          `Quick
          (repeat 1 generator_is_of_prime_order) ] )
end

module MakeEdwardsCurveProperties (G : Ec_sig.TwistedEdwardsT) = struct
  let test_elements_of_order_small_order () =
    let p1 =
      G.from_coordinates_exn
        ~u:(G.BaseField.of_string "0")
        ~v:(G.BaseField.of_string "1")
    in
    (* (0, -1) *)
    let p2 =
      G.from_coordinates_exn
        ~u:(G.BaseField.of_string "0")
        ~v:G.BaseField.(negate (of_string "1"))
    in

    let a_sqrt =
      Option.value ~default:G.BaseField.zero (G.BaseField.sqrt_opt G.a)
    in
    (* (a^-1/2, 0) *)
    let p3 =
      G.from_coordinates_exn
        ~u:G.BaseField.(inverse_exn a_sqrt)
        ~v:G.BaseField.(of_string "0")
    in

    (* ((-a)^-1/2, 0) *)
    let p4 =
      G.from_coordinates_exn
        ~u:G.BaseField.(negate (inverse_exn a_sqrt))
        ~v:G.BaseField.(of_string "0")
    in
    assert (G.(check_bytes (to_bytes p1))) ;
    assert (G.(check_bytes (to_bytes p2))) ;
    assert (G.(check_bytes (to_bytes p3))) ;
    assert (G.(check_bytes (to_bytes p4))) ;
    assert (G.(eq (mul p1 (ScalarField.of_string "1")) zero)) ;
    assert (G.(eq (mul p2 (ScalarField.of_string "2")) zero)) ;
    assert (G.(eq (mul p3 (ScalarField.of_string "4")) zero)) ;
    assert (G.(eq (mul p4 (ScalarField.of_string "4")) zero))

  let get_tests () =
    let open Alcotest in
    ( "Group properties",
      [ test_case
          "check elements of small orders"
          `Quick
          (repeat 1 test_elements_of_order_small_order) ] )
end
