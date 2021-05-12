(*
  https://ed25519.cr.yp.to/
  https://ed25519.cr.yp.to/python/ed25519.py
  https://ed25519.cr.yp.to/eddsa-20150704.pdf (page 4, examples)
*)
module Base = Ff.MakeFp (struct
  let prime_order = Z.(pow (of_int 2) 255 - of_int 19)
end)

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.(pow (of_int 2) 252 + of_string "27742317777372353535851937790883648493")
end)

module Affine = struct
  include Ec.MakeAffineEdwards (Base) (Scalar)
            (struct
              let a = Base.(negate (of_string "1"))

              let d = Base.(negate (of_string "121665" / of_string "121666"))

              (* https://ed25519.cr.yp.to/eddsa-20150704.pdf (page 4, examples).
                 c = 3 -> cofactor 8
              *)
              let cofactor = Z.of_string "8"

              (* https://ed25519.cr.yp.to/eddsa-20150704.pdf (page 4, examples).
                 > B is the point (... 202, 4/5). Calculed using:
                 ```
                 let f a d x = Base.(sqrt_opt ((one + negate (x * x)) / (a + (negate d) * x * x)));;
                 ```
                 And use the result of:
                 ```
                 Base.to_string @@ Base.negate @@ Option.get @@ f (Base.(negate one))
                 (Base.(negate (of_string "121665" / of_string "121666")))
                 (Base.(of_string "4" / of_string "5"));;
                 ```
              *)
              let bytes_generator =
                Bytes.concat
                  Bytes.empty
                  [ Base.(
                      to_bytes
                        (of_string
                           "15112221349535400772501151409588531511454012693041857206046113283949847762202"));
                    Base.(to_bytes (of_string "4" / of_string "5")) ]
            end)

  let of_compressed_opt b =
    (* required to avoid side effect! *)
    let b = Bytes.copy b in
    let length = Bytes.length b in
    if length <> 32 then None
    else
      (* We get the last bit of the input, representing the bit of u. We also
         remove the last bit from the bytes we received
      *)
      let last_byte = int_of_char @@ Bytes.get b (length - 1) in
      let sign = last_byte lsr 7 in
      let last_byte_without_sign = last_byte land 0b01111111 in
      Bytes.set b (length - 1) (char_of_int last_byte_without_sign) ;
      (* We compute u *)
      let v = Base.of_bytes_opt b in
      match v with
      | None -> None
      | Some v -> (
          (* Isolate u^2 *)
          (*      a u^2 + v^2 = 1 + d u^2 v^2 *)
          (* <==> a u^2 - d u^2 v^2 = 1 - v^2 *)
          (* <==> u^2 (a - d v^2) = 1 - v^2 *)
          (* <==> u^2 = (1 - v^2) / (a - d v^2)  *)
          let vv = Base.(v * v) in
          let dvv = Base.(d * vv) in
          let num = Base.(one + negate vv) in
          let den = Base.(a + negate dvv) in
          let uu = Base.(num / den) in
          let u_opt = Base.sqrt_opt uu in
          let u =
            match u_opt with
            | None -> None
            | Some u ->
                (* computed before for constant time *)
                let negated_u = Base.negate u in
                let u_first_byte = Bytes.get (Base.to_bytes u) 0 in
                let is_sign_flipped =
                  int_of_char u_first_byte lxor sign land 1
                in
                Some (if is_sign_flipped = 0 then u else negated_u)
          in
          match u with
          | Some u -> Some (unsafe_from_coordinates ~u ~v)
          | None -> None )

  let of_compressed_exn b =
    match of_compressed_opt b with
    | None -> raise (Not_on_curve b)
    | Some p -> p

  let to_compressed p =
    let u = get_u_coordinate p in
    let v = get_v_coordinate p in
    let u_bytes = Base.to_bytes u in
    let v_bytes = Base.to_bytes v in
    let u_first_byte = int_of_char (Bytes.get u_bytes 0) in
    let v_last_byte =
      int_of_char (Bytes.get v_bytes (Base.size_in_bytes - 1))
    in
    (* Get the first bit of u, i.e. the sign of u *)
    let sign_of_u = u_first_byte land 0b00000001 in
    (* Set the last bit of the last byte of v to the sign of u *)
    let v_last_byte_with_u = v_last_byte lor (sign_of_u lsl 7) in
    Bytes.set v_bytes (Base.size_in_bytes - 1) (char_of_int v_last_byte_with_u) ;
    v_bytes
end
