module type T = sig
  exception Not_on_curve of Bytes.t

  (** The type of the element in the elliptic curve *)
  type t

  (** The size of a point representation, in bytes *)
  val size_in_bytes : int

  module Scalar : Ff.T

  (** Create an empty value to store an element of the curve. DO NOT USE THIS TO
      DO COMPUTATIONS WITH, UNDEFINED BEHAVIORS MAY HAPPEN *)
  val empty : unit -> t

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

  (** Return true if the given element is zero *)
  val is_zero : t -> bool

  (** Generate a random element *)
  val random : ?state:Random.State.t -> unit -> t

  (** Return the addition of two element *)
  val add : t -> t -> t

  (** Double the element *)
  val double : t -> t

  (** Return the opposite of the element *)
  val negate : t -> t

  (** Return true if the two elements are algebraically the same *)
  val eq : t -> t -> bool

  (** Multiply an element by a scalar *)
  val mul : t -> Scalar.t -> t

  (** Return a string representation of the point *)
  val to_string : t -> string
end

module type AffineTwistedEdwardsT = sig
  include T

  module BaseField : Ff.T

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
