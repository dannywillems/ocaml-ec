(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2021 Danny Willems <be.danny.willems@gmail.com>             *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f )

let test_sign_and_verify_random_message () =
  let sk = Ec_curve25519.Affine.ScalarField.random () in
  let vk = Ec_curve25519.Affine.(mul one sk) in
  let message =
    Bytes.init (Random.int 100) (fun _ -> char_of_int (Random.int 256))
  in
  let signature = Ed25519.sign sk message in
  assert (Ed25519.verify vk message signature)

let test_sign_and_verify_wrong_message () =
  (* Sign a message and use the signature with a different message *)
  let sk = Ec_curve25519.Affine.ScalarField.random () in
  let vk = Ec_curve25519.Affine.(mul one sk) in
  let correct_message = Bytes.of_string "Correct message" in
  let incorrect_message = Bytes.of_string "Incorrect message" in
  let signature = Ed25519.sign sk correct_message in
  assert (not (Ed25519.verify vk incorrect_message signature))

let test_sign_and_verify_wrong_vk_because_of_different_sk () =
  (* Sign a message with a sk and attempt to verify with an invalid vk. The
     verifying key is computed using a different secret key, but the same generator
     than the scheme
  *)
  let sk = Ec_curve25519.Affine.ScalarField.random () in
  let vk =
    Ec_curve25519.Affine.(mul one (Ec_curve25519.Affine.ScalarField.random ()))
  in
  let message = Bytes.of_string "Message" in
  let signature = Ed25519.sign sk message in
  assert (not (Ed25519.verify vk message signature))

let test_sign_and_verify_wrong_vk_because_of_different_generator () =
  (* Sign a message with a sk and attempt to verify with an invalid vk. The vk
     is generated using the correct secret key but using a different generator than
     the one used in the scheme.
  *)
  let sk = Ec_curve25519.Affine.ScalarField.random () in
  let vk = Ec_curve25519.Affine.(mul (Ec_curve25519.Affine.random ()) sk) in
  let message = Bytes.of_string "Message" in
  let signature = Ed25519.sign sk message in
  assert (not (Ed25519.verify vk message signature))

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Ed25519"
    [ ( "Signature scheme properties",
        [ Alcotest.test_case
            "Sign and verify random message"
            `Quick
            (repeat 100 test_sign_and_verify_random_message);
          Alcotest.test_case
            "Sign and verify wrong message"
            `Quick
            (repeat 100 test_sign_and_verify_wrong_message);
          Alcotest.test_case
            "sign and verify with an invalid verifying key (different sk)"
            `Quick
            (repeat 100 test_sign_and_verify_wrong_vk_because_of_different_sk);
          Alcotest.test_case
            "sign and verify with an invalid verifying key (different \
             generator)"
            `Quick
            (repeat
               100
               test_sign_and_verify_wrong_vk_because_of_different_generator) ]
      ) ]
