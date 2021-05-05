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

module RedJubjub = Redjubjub.Make (struct
  let generator = Ec_jubjub.Affine.one
end)

let test_sign_and_verify_random_message () =
  let sk = Ec_jubjub.Affine.ScalarField.random () in
  let vk = Ec_jubjub.Affine.(mul one sk) in
  let message =
    Bytes.init (Random.int 100) (fun _ -> char_of_int (Random.int 256))
  in
  let signature = RedJubjub.sign sk message in
  assert (RedJubjub.verify vk message signature)

let test_sign_and_verify_wrong_message () =
  let sk = Ec_jubjub.Affine.ScalarField.random () in
  let vk = Ec_jubjub.Affine.(mul one sk) in
  let correct_message = Bytes.of_string "Correct message" in
  let incorrect_message = Bytes.of_string "Incorrect message" in
  let signature = RedJubjub.sign sk correct_message in
  assert (not (RedJubjub.verify vk incorrect_message signature))

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Redjubjub"
    [ ( "Sign and verify random message",
        [ Alcotest.test_case
            "Sign and verify random message"
            `Quick
            (repeat 100 test_sign_and_verify_random_message);
          Alcotest.test_case
            "Sign and verify wrong message"
            `Quick
            (repeat 100 test_sign_and_verify_wrong_message) ] ) ]
