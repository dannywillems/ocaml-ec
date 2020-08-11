module type T = sig
  exception Not_on_curve of Bytes.t

  (** The type of the element in the elliptic curve *)
  type t

  val size_in_bytes : int
  (** The size of a point representation, in bytes *)

  module Scalar : Ff.T

  val empty : unit -> t
  (** Create an empty value to store an element of the curve. DO NOT USE THIS TO
      DO COMPUTATIONS WITH, UNDEFINED BEHAVIORS MAY HAPPEN *)

  val check_bytes : Bytes.t -> bool
  (** Check if a point, represented as a byte array, is on the curve **)

  val of_bytes_opt : Bytes.t -> t option
  (** Attempt to construct a point from a byte array *)

  val of_bytes_exn : Bytes.t -> t
  (** Attempt to construct a point from a byte array.
      Raise [Not_on_curve] if the point is not on the curve
  *)

  val to_bytes : t -> Bytes.t
  (** Return a representation in bytes *)

  val zero : t
  (** Zero of the elliptic curve *)

  val one : t
  (** A fixed generator of the elliptic curve *)

  val is_zero : t -> bool
  (** Return true if the given element is zero *)

  val random : ?state:Random.State.t -> unit -> t
  (** Generate a random element *)

  val add : t -> t -> t
  (** Return the addition of two element *)

  val negate : t -> t
  (** Return the opposite of the element *)

  val eq : t -> t -> bool
  (** Return true if the two elements are algebraically the same *)

  val mul : t -> Scalar.t -> t
  (** Multiply an element by a scalar *)
end
