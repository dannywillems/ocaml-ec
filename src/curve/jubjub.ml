(**
  https://github.com/daira/jubjub

  Base field: 52435875175126190479447740508185965837690552500527637822603658699938581184513 (254 bits - 32 bytes)
  Scalar field: 6554484396890773809930967563523245729705921265872317281365359162392183254199 (251 bits - 32 bytes)

  Base field multiplicative subgroup decomposition:
    2^32 * 3 * 11 * 19 * 10177 * 125527 * 859267 * 906349^2 * 2508409 * 2529403 * 52437899 * 254760293^2

  Prime field multiplication subgroup decomposition:
    2 * 3 * 12281 * 88951556562858260862727893541829461901934170206990707615630637602694993
*)
module Base = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "6554484396890773809930967563523245729705921265872317281365359162392183254199"
end)

module Affine : sig
  include Ec_sig.AffineEdwardsT

  val of_compressed_exn : Bytes.t -> t

  val of_compressed_opt : Bytes.t -> t option

  val to_compressed : t -> Bytes.t
end = struct
  include Ec.MakeAffineEdwards (Base) (Scalar)
            (struct
              let a = Base.(negate one)

              let d =
                Base.of_string
                  "19257038036680949359750312669786877991949435402254120286184196891950884077233"

              let cofactor = Z.of_string "8"

              let bytes_generator =
                Bytes.concat
                  Bytes.empty
                  [ Base.(
                      to_bytes
                        (of_string
                           "8076246640662884909881801758704306714034609987455869804520522091855516602923"));
                    Base.(
                      to_bytes
                        (of_string
                           "13262374693698910701929044844600465831413122818447359594527400194675274060458"))
                  ]
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
