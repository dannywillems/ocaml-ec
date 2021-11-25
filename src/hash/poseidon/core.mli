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
end

module type FIXED_INPUT_LENGTH_PARAMETERS = sig
  include PARAMETERS

  val fixed_input_length : int
end

module MakeVariableLengthInput : functor
  (C : PARAMETERS)
  (Scalar : Ff_sig.PRIME)
  -> sig
  type scalar

  type ctxt

  val init : scalar array -> ctxt

  val apply_perm : ctxt -> ctxt

  val get : ctxt -> scalar

  val digest : scalar array -> scalar
end

module MakeFixedLengthInput : functor
  (C : FIXED_INPUT_LENGTH_PARAMETERS)
  (Scalar : Ff_sig.PRIME)
  -> sig
  type scalar

  type ctxt

  val digest : scalar array -> ctxt

  val get : ctxt -> scalar
end
