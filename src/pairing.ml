module type T = sig
  module G1 : Elliptic_curve.T

  module G2 : Elliptic_curve.T

  module GT : Ff.T

  exception FailToComputeFinalExponentiation of GT.t

  val miller_loop : (G1.t * G2.t) list -> GT.t

  (** Compute the miller loop on a single tuple of point *)
  val miller_loop_simple : G1.t * G2.t -> GT.t

  (** Compute a pairing result of a list of points *)
  val pairing : (G1.t * G2.t) list -> GT.t

  (** Compute the final exponentiation of the given point. Returns a [None] if
      the point is null *)
  val final_exponentiation_opt : GT.t -> GT.t option

  (** Compute the final exponentiation of the given point. Raise
      [FailToComputeFinalExponentiation] if the point is null *)
  val final_exponentiation_exn : GT.t -> GT.t
end
