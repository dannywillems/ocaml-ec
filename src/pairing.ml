module type T = sig
  module G1 : Elliptic_curve.T

  module G2 : Elliptic_curve.T

  module GT : Ff.T

  val miller_loop : (G1.t * G2.t) list -> GT.t

  val miller_loop_simple : G1.t * G2.t -> GT.t

  val pairing : (G1.t * G2.t) list -> GT.t

  val final_exponentiation_opt : GT.t -> GT.t option

  val final_exponentiation_exn : GT.t -> GT.t option
end
