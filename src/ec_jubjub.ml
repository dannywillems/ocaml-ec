(* https://github.com/daira/jubjub *)
(* Check it is not a small order element *)

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

module TwistedEdwards : sig
  include Ec_sig.TwistedEdwardsT
end = struct
  include Ec.MakeTwistedEdwards (Base) (Scalar)
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
end
