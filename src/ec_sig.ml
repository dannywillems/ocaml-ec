(** Basic interface for elliptic curves *)
module type BASE = sig
  exception Not_on_curve of Bytes.t

  (** The type of the element in the elliptic curve *)
  type t

  (** The size of a point representation, in bytes *)
  val size_in_bytes : int

  module ScalarField : Ff_sig.PRIME

  module BaseField : Ff_sig.PRIME

  (** Check if a point, represented as a byte array, is on the curve **)
  val check_bytes : Bytes.t -> bool

  (** Attempt to construct a point from a byte array *)
  val of_bytes_opt : Bytes.t -> t option

  (** Attempt to construct a point from a byte array.
      Raise [Not_on_curve] if the point is not on the curve
  *)
  val of_bytes_exn : Bytes.t -> t

  (** Return a representation in bytes *)
  val to_bytes : t -> Bytes.t

  (** Zero of the elliptic curve *)
  val zero : t

  (** A fixed generator of the elliptic curve *)
  val one : t

  (** Return [true] if the given element is zero *)
  val is_zero : t -> bool

  (** Generate a random element *)
  val random : ?state:Random.State.t -> unit -> t

  (** Return the addition of two element *)
  val add : t -> t -> t

  (** Double the element *)
  val double : t -> t

  (** Return the opposite of the element *)
  val negate : t -> t

  (** Return [true] if the two elements are algebraically the same *)
  val eq : t -> t -> bool

  (** Multiply an element by a scalar *)
  val mul : t -> ScalarField.t -> t
end

(** Curve in Weierstrass form with a and b. In affine, the curve has the
    equation form y² = x³ + ax + b *)
module type WeierstrassT = sig
  include BASE

  val a : BaseField.t

  val b : BaseField.t
end

module type AffineWeierstrassT = sig
  include WeierstrassT

  val get_x_coordinate : t -> BaseField.t

  val get_y_coordinate : t -> BaseField.t

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, returns [None]
  *)
  val from_coordinates_opt : x:BaseField.t -> y:BaseField.t -> t option

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, raise [Not_on_curve].
  *)
  val from_coordinates_exn : x:BaseField.t -> y:BaseField.t -> t
end

module type ProjectiveWeierstrassT = sig
  include WeierstrassT

  val get_x_coordinate : t -> BaseField.t

  val get_y_coordinate : t -> BaseField.t

  val get_z_coordinate : t -> BaseField.t

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, returns [None]
  *)
  val from_coordinates_opt :
    x:BaseField.t -> y:BaseField.t -> z:BaseField.t -> t option

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, raise [Not_on_curve].
  *)
  val from_coordinates_exn :
    x:BaseField.t -> y:BaseField.t -> z:BaseField.t -> t
end

module type TwistedEdwardsT = sig
  (** ax^2 + y^2 = 1 + dx^2y^2 *)
  include BASE

  val a : BaseField.t

  val d : BaseField.t

  (** Return the affine coordinate u (such that -u^2 + v^2 = 1 + d u^2 v^2 *)
  val get_u_coordinate : t -> BaseField.t

  (** Return the affine coordinate u (such that -u^2 + v^2 = 1 + d u^2 v^2 *)
  val get_v_coordinate : t -> BaseField.t

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, returns [None]
  *)
  val from_coordinates_opt : u:BaseField.t -> v:BaseField.t -> t option

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, raise [Not_on_curve].
  *)
  val from_coordinates_exn : u:BaseField.t -> v:BaseField.t -> t
end

module type PAIRING = sig
  module G1 : BASE

  module G2 : BASE

  module GT : Ff_sig.BASE

  exception FailToComputeFinalExponentiation of GT.t

  val miller_loop : (G1.t * G2.t) list -> GT.t

  (** Compute the miller loop on a single tuple of point *)
  val miller_loop_simple : G1.t -> G2.t -> GT.t

  (** Compute a pairing result of a list of points *)
  val pairing : G1.t -> G2.t -> GT.t

  (** Compute the final exponentiation of the given point. Returns a [None] if
      the point is null *)
  val final_exponentiation_opt : GT.t -> GT.t option

  (** Compute the final exponentiation of the given point. Raise
      [FailToComputeFinalExponentiation] if the point is null *)
  val final_exponentiation_exn : GT.t -> GT.t
end
