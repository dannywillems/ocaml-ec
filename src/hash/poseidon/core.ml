module type PARAMETERS = sig
  val width : int

  val full_rounds : int

  val partial_rounds : int

  val round_constants : string array

  val mds_matrix : string array array

  val partial_round_idx_to_permute : int
end

module type STRATEGY = sig
  type scalar

  type state

  val init : scalar array -> state

  val apply_perm : state -> unit

  val get : state -> scalar array
end

module type HASH = sig
  type scalar

  type ctxt

  val init : unit -> ctxt

  val digest : ctxt -> scalar array -> ctxt

  val get : ctxt -> scalar
end

module Make (C : PARAMETERS) (Scalar : Ff_sig.PRIME) = struct
  open C

  (* Verify the constants are consistent *)
  let () =
    assert (Array.length mds_matrix = width) ;
    assert (Array.for_all (fun line -> Array.length line = width) mds_matrix)

  let mds_matrix = Array.map (Array.map Scalar.of_string) mds_matrix

  let round_constants = Array.map Scalar.of_string round_constants

  (* Initialize only once an array for the MDS matrix multiplication *)
  let res = Array.make width Scalar.zero

  module Strategy = struct
    type scalar = Scalar.t

    type state = { mutable i_round_key : int; state : Scalar.t array }

    let init state = { i_round_key = 0; state = Array.copy state }

    let get_next_round_key s =
      let v = round_constants.(s.i_round_key) in
      s.i_round_key <- s.i_round_key + 1 ;
      v

    let s_box x = Scalar.(square (square x) * x)

    (* Functions prefixed with apply_ are modifying the state given in
       parameters
    *)
    let apply_round_key s =
      let state = s.state in
      for i = 0 to Array.length state - 1 do
        state.(i) <- Scalar.(get_next_round_key s + state.(i))
      done

    let apply_s_box_last_elem s =
      let s = s.state in
      s.(partial_round_idx_to_permute) <- s_box s.(partial_round_idx_to_permute)

    let apply_s_box s =
      let s = s.state in
      for i = 0 to Array.length s - 1 do
        s.(i) <- s_box s.(i)
      done

    let apply_eval_matrix m v =
      let v = v.state in
      for j = 0 to width - 1 do
        for k = 0 to width - 1 do
          res.(k) <- Scalar.(res.(k) + (m.(k).(j) * v.(j)))
        done
      done ;
      for j = 0 to width - 1 do
        v.(j) <- res.(j) ;
        res.(j) <- Scalar.zero
      done

    let apply_partial_round s =
      apply_round_key s ;
      apply_s_box_last_elem s ;
      apply_eval_matrix mds_matrix s

    let apply_full_round s =
      apply_round_key s ;
      apply_s_box s ;
      apply_eval_matrix mds_matrix s

    let apply_perm s =
      s.i_round_key <- 0 ;
      for _i = 0 to (full_rounds / 2) - 1 do
        apply_full_round s
      done ;
      for _i = 0 to partial_rounds - 1 do
        apply_partial_round s
      done ;
      for _i = 0 to (full_rounds / 2) - 1 do
        apply_full_round s
      done

    let get s = Array.copy s.state

    let add_cst s idx v =
      assert (idx <= width) ;
      s.state.(idx) <- Scalar.(s.state.(idx) + v)
  end

  module Hash = struct
    type scalar = Scalar.t

    type ctxt = Strategy.state

    let init () =
      let state = Strategy.init (Array.make width Scalar.zero) in
      state

    let digest state data =
      let l = Array.length data in
      let chunk_size = width - 1 in
      let nb_full_chunk = l / chunk_size in
      let r = l mod chunk_size in
      (* we process first all the full chunks *)
      for i = 0 to nb_full_chunk - 1 do
        let ofs = i * chunk_size in
        for j = 0 to chunk_size - 1 do
          Strategy.add_cst state (1 + j) data.(ofs + j)
        done ;
        Strategy.apply_perm state
      done ;
      (* we add the last partial chunk, add pad with one *)
      for j = 0 to r - 1 do
        let idx = 1 + j in
        Strategy.add_cst state idx data.((nb_full_chunk * chunk_size) + j)
      done ;
      Strategy.add_cst state (r + 1) Scalar.one ;
      Strategy.apply_perm state ;
      state

    let get (ctxt : ctxt) = ctxt.state.(1)
  end
end

module type FF_WITH_INPLACE = sig
  include Ff_sig.PRIME

  val copy : t -> t

  val add_inplace : t -> t -> unit

  val mul_inplace : t -> t -> unit
end

module MakeInplace (C : PARAMETERS) (Scalar : FF_WITH_INPLACE) = struct
  open C

  (* Verify the constants are consistent *)
  let () =
    assert (Array.length mds_matrix = width) ;
    assert (Array.for_all (fun line -> Array.length line = width) mds_matrix)

  let mds_matrix = Array.map (Array.map Scalar.of_string) mds_matrix

  let round_constants = Array.map Scalar.of_string round_constants

  (* Initialize only once an array for the MDS matrix multiplication *)
  let res = Array.init width (fun _ -> Scalar.(copy zero))

  module Strategy = struct
    type scalar = Scalar.t

    type state = { mutable i_round_key : int; state : Scalar.t array }

    let init state =
      { i_round_key = 0;
        state = Array.init width (fun i -> Scalar.(copy state.(i)))
      }

    let get_next_round_key s =
      let v = round_constants.(s.i_round_key) in
      s.i_round_key <- s.i_round_key + 1 ;
      v

    let s_box x =
      let x_copy = Scalar.copy x in
      Scalar.mul_inplace x x ;
      Scalar.mul_inplace x x ;
      Scalar.mul_inplace x x_copy ;
      x

    (* Functions prefixed with apply_ are modifying the state given in
       parameters
    *)
    let apply_round_key s =
      let state = s.state in
      for i = 0 to Array.length state - 1 do
        Scalar.add_inplace state.(i) (get_next_round_key s)
      done

    let apply_s_box_last_elem s =
      let s = s.state in
      s.(partial_round_idx_to_permute) <- s_box s.(partial_round_idx_to_permute)

    let apply_s_box s =
      let s = s.state in
      for i = 0 to Array.length s - 1 do
        s.(i) <- s_box s.(i)
      done

    let apply_eval_matrix m v =
      let v = v.state in
      for j = 0 to width - 1 do
        for k = 0 to width - 1 do
          Scalar.add_inplace res.(k) Scalar.(m.(k).(j) * v.(j))
        done
      done ;
      for j = 0 to width - 1 do
        v.(j) <- res.(j) ;
        res.(j) <- Scalar.(copy zero)
      done

    let apply_partial_round s =
      apply_round_key s ;
      apply_s_box_last_elem s ;
      apply_eval_matrix mds_matrix s

    let apply_full_round s =
      apply_round_key s ;
      apply_s_box s ;
      apply_eval_matrix mds_matrix s

    let apply_perm s =
      s.i_round_key <- 0 ;
      for _i = 0 to (full_rounds / 2) - 1 do
        apply_full_round s
      done ;
      for _i = 0 to partial_rounds - 1 do
        apply_partial_round s
      done ;
      for _i = 0 to (full_rounds / 2) - 1 do
        apply_full_round s
      done

    let get s = Array.copy s.state

    let add_cst s idx v =
      assert (idx <= width) ;
      Scalar.add_inplace s.state.(idx) v
  end

  module Hash = struct
    type scalar = Scalar.t

    type ctxt = Strategy.state

    let init () =
      let state =
        Strategy.init (Array.init width (fun _ -> Scalar.(copy zero)))
      in
      state

    let digest state data =
      let l = Array.length data in
      let chunk_size = width - 1 in
      let nb_full_chunk = l / chunk_size in
      let r = l mod chunk_size in
      (* we process first all the full chunks *)
      for i = 0 to nb_full_chunk - 1 do
        let ofs = i * chunk_size in
        for j = 0 to chunk_size - 1 do
          Strategy.add_cst state (1 + j) data.(ofs + j)
        done ;
        Strategy.apply_perm state
      done ;
      (* we add the last partial chunk, add pad with one *)
      for j = 0 to r - 1 do
        let idx = 1 + j in
        Strategy.add_cst state idx data.((nb_full_chunk * chunk_size) + j)
      done ;
      Strategy.add_cst state (r + 1) Scalar.one ;
      Strategy.apply_perm state ;
      state

    let get (ctxt : ctxt) = ctxt.state.(1)
  end
end
