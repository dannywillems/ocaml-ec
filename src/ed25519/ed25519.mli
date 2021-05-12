type secret_key = Ec_curve25519.Affine.ScalarField.t

type public_key = Ec_curve25519.Affine.t

type signature

val signature_to_bytes : signature -> Bytes.t

val sign : secret_key -> Bytes.t -> signature

val verify : public_key -> Bytes.t -> signature -> bool
