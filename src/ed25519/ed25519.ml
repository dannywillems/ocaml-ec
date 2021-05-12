type secret_key = Ec_curve25519.Affine.ScalarField.t

type public_key = Ec_curve25519.Affine.t

type signature = { r : Bytes.t; s : Bytes.t }

let sha512 b =
  let ctx = Digestif.SHA512.init () in
  let ctx = Digestif.SHA512.feed_bytes ctx b in
  let ctx = Digestif.SHA512.(to_hex (get ctx)) in
  let hash_hex = `Hex ctx in
  let hash_bytes = Hex.to_bytes hash_hex in
  hash_bytes

let sha512_to_scalar b =
  let b = sha512 b in
  Ec_curve25519.Affine.ScalarField.of_bytes_exn b

let secret_expand sk =
  assert (Bytes.length sk = 32) ;
  let h = sha512 sk in
  let a = Z.of_bits (Bytes.to_string (Bytes.sub h 0 32)) in
  (* Clear the last two bits *)
  let i1 = 1 lsl 254 in
  let i2 = i1 - 8 in
  let a = Z.(a land of_int i2) in
  let a = Z.(a lor of_int i1) in
  (Ec_curve25519.Affine.ScalarField.of_z a, Bytes.sub h 32 32)

let _secret_to_public sk =
  let (a, _dummy) = secret_expand sk in
  Ec_curve25519.Affine.to_compressed
    (Ec_curve25519.Affine.mul Ec_curve25519.Affine.one a)

let signature_to_bytes signature =
  ignore signature ;
  Bytes.empty

let sign sk msg =
  let (a, prefix) =
    secret_expand (Ec_curve25519.Affine.ScalarField.to_bytes sk)
  in
  let vk = Ec_curve25519.Affine.mul Ec_curve25519.Affine.one a in
  let vk_bytes = Ec_curve25519.Affine.to_bytes vk in
  let random_scalar =
    sha512_to_scalar (Bytes.concat Bytes.empty [prefix; msg])
  in
  let random_point = Ec_curve25519.Affine.(mul one random_scalar) in
  let random_bytes = Ec_curve25519.Affine.to_compressed random_point in
  let h =
    sha512_to_scalar (Bytes.concat Bytes.empty [random_bytes; vk_bytes; msg])
  in
  let s = Ec_curve25519.Affine.ScalarField.(random_scalar + (h * a)) in
  { r = random_bytes; s = Ec_curve25519.Affine.ScalarField.to_bytes s }

let verify vk msg signature =
  let r = Ec_curve25519.Affine.of_compressed_exn signature.r in
  let s = Ec_curve25519.Affine.ScalarField.of_bytes_exn signature.s in
  let vk_bytes = Ec_curve25519.Affine.to_compressed vk in
  let h =
    sha512_to_scalar (Bytes.concat Bytes.empty [signature.r; vk_bytes; msg])
  in
  let s_b = Ec_curve25519.Affine.(mul one s) in
  let h_a = Ec_curve25519.Affine.(mul r h) in
  Ec_curve25519.Affine.(eq s_b (add r h_a))
