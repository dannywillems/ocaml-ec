module Constants = Constants
open Constants

module Make (Scalar : sig
  include Ff_sig.PRIME

  val add_inplace : t -> t -> unit

  val mul_inplace : t -> t -> unit

  val copy : t -> t
end) =
struct
  (* Initialize only once an array for the MDS matrix multiplication *)
  let res = Array.init width (fun _ -> Scalar.zero)

  (* simply verify size_in_bytes in Scalar is correct so we can use it everywhere,
     specifically when reading the mds binary file. This check is here because
     the parameters are not given in the functor yet.
  *)
  let () = assert (Scalar.size_in_bytes = 32)

  let mds_matrix =
    Array.map (fun l -> Array.map (fun s -> Scalar.of_string s) l) Mds.v

  let round_constants = Array.map (fun s -> Scalar.of_string s) Ark.v

  module Strategy = struct
    type state = { mutable i_round_key : int; state : Scalar.t array }

    let init state = { i_round_key = 0; state = Array.copy state }

    let get_next_round_key s =
      let v = round_constants.(s.i_round_key) in
      s.i_round_key <- s.i_round_key + 1 ;
      v

    (* let s_box x = Scalar.(square (square x) * x) *)

    let s_box_inplace x =
      (* Printf.printf "x: %s\n" (Scalar.to_string x) ; *)
      let c_x = Scalar.copy x in
      (* Printf.printf "c_x: %s\n" (Scalar.to_string c_x) ; *)
      Scalar.mul_inplace x x ;
      Scalar.mul_inplace x x ;
      Scalar.mul_inplace x c_x

    (* Functions prefixed with apply_ are modifying the state given in
       parameters
    *)
    let apply_round_key s =
      let state = s.state in
      for i = 0 to Array.length state - 1 do
        let next_round_key = get_next_round_key s in
        (* let res = Scalar.add state.(i) next_round_key in *)
        (* Printf.printf
         *   "Res: %s\nInit value of state.(i): %s\nround_key_index: %d\n"
         *   (Scalar.to_string res)
         *   (Scalar.to_string state.(i))
         *   s.i_round_key ; *)
        Scalar.(add_inplace state.(i) next_round_key)
        (* state.(i) <- res *)
        (* Printf.printf "After inplace: %s\n" (Scalar.to_string state.(i)) *)
        (* state.(i) <- Scalar.(get_next_round_key s + state.(i)) *)
      done

    let apply_s_box_last_elem s =
      let s = s.state in
      let last_elem_idx = Array.length s - 1 in
      (* s.(last_elem_idx) <- s_box s.(last_elem_idx) *)
      s_box_inplace s.(last_elem_idx)

    let apply_s_box s =
      let s = s.state in
      for i = 0 to Array.length s - 1 do
        (* s.(i) <- s_box s.(i) *)
        s_box_inplace s.(i)
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
      Scalar.add_inplace s.state.(idx) v
  end

  module Hash = struct
    type ctxt = Strategy.state

    let init () =
      let state =
        Strategy.init (Array.init width (fun _ -> Scalar.of_string "0"))
      in
      state

    let hash state d =
      let l = Array.length d in
      let chunk_size = width - 1 in
      let nb_full_chunk = l / chunk_size in
      let r = l mod chunk_size in
      (* we process first all the full chunks *)
      for i = 0 to nb_full_chunk - 1 do
        let ofs = i * chunk_size in
        for j = 0 to chunk_size - 1 do
          Strategy.add_cst state (1 + j) d.(ofs + j)
        done ;
        Strategy.apply_perm state
      done ;
      (* we add the last partial chunk, add pad with one *)
      for j = 0 to r - 1 do
        let idx = 1 + j in
        Strategy.add_cst state idx d.((nb_full_chunk * chunk_size) + j)
      done ;
      Strategy.add_cst state (r + 1) Scalar.one ;
      Strategy.apply_perm state ;
      state

    let get (ctxt : ctxt) = ctxt.state.(1)
  end
end
