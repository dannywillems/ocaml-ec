module type PARAMETERS = sig
  (** The state size *)
  val width : int

  (** The total number of full rounds *)
  val full_rounds : int

  (** The number of partial rounds *)
  val partial_rounds : int

  (** The round constants, given in decimal representation *)
  val round_constants : string array

  (** The MDS matrix, given in decimal representation *)
  val mds_matrix : string array array

  (** The index of the element of the state to permute during the partial rounds *)
  val partial_round_idx_to_permute : int

  (** Boolean to specify the use of padding (no padding should only be used on fixed-length inputs) *)
  val with_padding : bool
end

(** A HADES strategy, for a constant length construction *)
module type STRATEGY = sig
  type scalar

  (** The state of the strategy *)
  type state

  (** Initialize the state with the given input. The input must be the same length than the width *)
  val init : scalar array -> state

  (** Apply a permutation round *)
  val apply_perm : state -> unit

  (** Return the current scalar elements in the state *)
  val get : state -> scalar array
end

module type HASH = sig
  type scalar

  type ctxt

  (** Initialize a raw hash context *)
  val init : unit -> ctxt

  (** [hash ctxt input] computes the hash of the given input. The input must be
      of length [width - 1]
  *)
  val digest : ctxt -> scalar array -> ctxt

  (** [get ctxt] returns the resulting point after [hash] has been called *)
  val get : ctxt -> scalar
end

module Make : functor (C : PARAMETERS) (Scalar : Ff_sig.PRIME) -> sig
  module Strategy : STRATEGY with type scalar = Scalar.t

  module Hash : HASH with type scalar = Scalar.t
end
