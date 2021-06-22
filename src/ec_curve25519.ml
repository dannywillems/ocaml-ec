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

module Affine =
  Ec.MakeAffineEdwards (Base) (Scalar)
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
