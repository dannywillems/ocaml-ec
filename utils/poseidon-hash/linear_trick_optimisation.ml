let compute_alpha prime_order =
  let rec aux alpha =
    let r, _, _ = Z.gcdext (Z.pred prime_order) alpha in
    if Z.(equal r one) then alpha else aux (Z.succ alpha)
  in
  aux (Z.of_int 3)

let naive_implementation alpha rf rp width =
  let nb_mul_per_exp = Z.log2 alpha + 1 in
  let nb_exp = (width * rf) + rp in
  let width_square = width * width in
  let mds_mul_full = rf * width_square in
  let mds_add_full = rf * (width * (width - 1)) in
  let arc_add_full = rf * width in

  let mds_mul_partial = rp * width_square in
  let mds_add_partial = rp * (width * (width - 1)) in
  let arc_add_partial = rp * width in

  Printf.printf "Partial add batch %d\n" (mds_add_partial + arc_add_partial) ;

  let mds_mul = mds_mul_full + mds_mul_partial in
  let mds_add = mds_add_full + mds_add_partial in
  let arc_add = arc_add_full + arc_add_partial in

  let mul = mds_mul + (nb_mul_per_exp * nb_exp) in
  let add = mds_add + arc_add in
  (add, mul)

let linear_trick alpha batch_size rf rp width =
  let nb_mul_per_exp = Z.log2 alpha + 1 in
  let width_square = width * width in

  (* precomputation cost *)
  (* TODO *)

  (* Full rounds - like naive implementation *)
  let nb_exp_full = width * rf in
  let mds_mul_full = rf * width_square in
  let mds_add_full = rf * (width * (width - 1)) in
  let arc_add_full = rf * width in

  (* Nb constant per batch
     1.if batch_size >= 2:
         width * nb_var_temp + nb_var_temp + (batch_size - 2) * (batch_size - 1) / 2

     2. width * width + width + nb_var_temp * width

     ==> width * (width + 2 * nb_var_temp + 1) + nb_var_temp + 1 + batch_size ^ 2 / 2 - 3/2 batch_size
     ==> width * (width + 2 batch_size - 1) - 1/2 batch_size + batch_size ^ 2 / 2
     ==> width * (width + 2 batch_size - 1) + 1/2 batch_size * (batch_size - 1)

     NB: width      -> quadratic
         batch_size -> quadratic

     Equations for the number of multiplications. Let's
     - W = width
     - BS = batch size
     - ME = Multplication per Exponentiation

     1. Addition
     1.1 Per batch:

                       --- for kappa in 2. ----  - for kappa in 1.
                       |                      |  |               |
      = CSTS_PER_BATCH -         W            -       (BS - 1)
      = W * (W + 2 * BS - 1) + 1/2 BS * (BS - 1) - W - (BS - 1)
      = W^2 + 2 * BS * W - 2 * W + 1/2 BS^2 - 3/2 BS + 1
      = W^2 + 1/2 BS^2 + 2 BS * W - 2 * W - 3/2 BS + 1

     1.2 Total
     Let's forget about the "normal residual partial rounds", we get:

     NB_ADD_PARTIAL = NB_ADD_PER_BATCH * (Rp / BS)
                    = (W^2 / BS + 1/2 BS + 2 W - 2 * W / BS - 3/2 + 1/BS) * Rp

     For W = 3 and Rp = 56, we have:
                    = (9 / BS + 1/2 BS + 6 - 6 / BS - 3/2 + 1/BS) * Rp
                    = (4 / BS + 1/2 BS - 9/2) * Rp
     As we want to minimize the number of additions, let's derive:
                    = (-4 / BS^2 + 1/2)
     For this instance, we have a minimum of additions around 3.

     2. Multiplication

     2.1 Per batch
     = Addition + ME * BS

     2.2 Total
     = NB_ADD_PARTIAL + ME * Rp
  *)
  (* Partial rounds

          For a batch of 4 and a width of 3, we create 3 intermediary variables
         (a, b, c) that will be used to compute the final batch state (y1, y2, y3)

         1. Temporary variable generations (i.e. batching)
                   ----------- Batch square ---------------------    --- Batch triangle ---
                   |                                            |    |                    |
                   -- a = \a_1 * x1 + \a_2 * x2 + \a_3 * x3^5 + k_a
     nb_var_temp  |   b = \b_1 * x1 + \b_2 * x2 + \b_3 * x3^5 + k_b  + \b_a * a^5
                   -- c = \c_1 * x1 + \c_2 * x2 + \c_3 * x3^5 + k_c  + \c_a * a^5 + \c_b * b^5
                           |                              |
                           ------------ width -------------

         2. Final state computation (before next batch or full round)
                     --------  Final (width) -----------      -----  Final (nb_var_temp) ---
                     |                                 |      |                            |
           -- y1 = \d_11 x1 + \d_12 x2 + \d_13 x3^5 + k_y1 + \d_1a a^5 + \d_1b b^5 + \d_1c c^5
     width |  y2 = \d_21 x1 + \d_22 x2 + \d_23 x3^5 + k_y2 + \d_2a a^5 + \d_2b b^5 + \d_2c c^5
           -- y3 = \d_31 x1 + \d_32 x2 + \d_33 x3^5 + k_y3 + \d_3a a^5 + \d_3b b^5 + \d_3c c^5
                      |                         |
                      ----------- width ---------

           "Final width" corresponds what we would do without batching (but with a different value of \d's.
           "Final nb_var_temp" corresponds to the batching "overhead"
  *)
  let nb_var_temp = batch_size - 1 in
  let length_triangle = max 0 (nb_var_temp - 1) in

  (* = triangle area *)
  let batch_triangle = length_triangle * (length_triangle + 1) / 2 in
  (*                 triangle    +      square                       *)
  let batch_add = batch_triangle + (width * nb_var_temp) in
  let batch_mul =
    (*   triangle    +      square         +       exponentiation    *)
    batch_triangle + (width * nb_var_temp) + (nb_mul_per_exp * nb_var_temp)
  in

  let final_add = (width * width) + (width * nb_var_temp) in
  (* For the last temp variable here c *)
  let cost_last_tmp_var_exp = nb_mul_per_exp in
  let final_mul =
    (width * width) + (width * nb_var_temp) + cost_last_tmp_var_exp
  in

  let nb_batch = rp / batch_size in
  let total_add_batch = nb_batch * (batch_add + final_add) in
  let total_mul_batch = nb_batch * (batch_mul + final_mul) in

  let nb_not_batch = rp mod batch_size in
  let add_not_batch = nb_not_batch * (width * (width - 1)) in
  let mul_not_batch = nb_not_batch * nb_mul_per_exp in

  (* total *)
  let add = mds_add_full + arc_add_full + total_add_batch + add_not_batch in
  let mul =
    mds_mul_full
    + (nb_exp_full * nb_mul_per_exp)
    + total_mul_batch + mul_not_batch
  in
  (* FIXME: Matrix addition cost *)
  (* Additional memory cost, as we keep the tmp var and the last state element
     raised to the power 5. *)
  let add_memory_cost = 1 + nb_var_temp in
  (add, mul, add_memory_cost)

let read_file (type a) (module Fp : Ff_sig.PRIME with type t = a) filename
    nb_line : a array =
  let chan = open_in filename in
  let read_line () =
    try Fp.of_string (input_line chan)
    with End_of_file ->
      close_in chan ;
      failwith "File does not contain enough values"
  in
  Array.init nb_line (fun _ -> read_line ())

let command =
  Core.Command.basic
    ~summary:"Compute number of operations for a given instance of Hades"
    ~readme:(fun () -> "TODO")
    Core.Command.Let_syntax.(
      let%map_open r_p = anon ("r_p" %: int)
      and r_f = anon ("r_f" %: int)
      and width = anon ("width" %: int)
      and batch_size = anon ("batch-size" %: int)
      and field_size = anon ("field-size" %: string)
      and ark_poseidon128_filename = anon ("ark-poseidon128-filename" %: string)
      and mds_poseidon128_filename =
        anon ("mds-poseidon128-filename" %: string)
      in
      fun () ->
        let field_size = Z.of_string field_size in
        let module Fp = Ff.MakeFp (struct
          let prime_order = field_size
        end) in
        let alpha = compute_alpha field_size in
        let naive_add, naive_mul = naive_implementation alpha r_f r_p width in
        let opt_add, opt_mul, _add_mem =
          linear_trick alpha batch_size r_f r_p width
        in
        Printf.printf
          "Naive implementation - %d add ; %d mul\n"
          naive_add
          naive_mul ;
        Printf.printf
          "Linear trick implementation - %d add ; %d mul\n"
          opt_add
          opt_mul ;
        Printf.printf
          "Gain/Loss - %d add ; %d mul\n"
          (naive_add - opt_add)
          (naive_mul - opt_mul) ;
        (* We retrieve the ARC. *)
        let nb_arc = (r_p + r_f) * width in
        let arc = read_file (module Fp) ark_poseidon128_filename nb_arc in
        (* We format the MDS in a matrix. *)
        let nb_mds = width * width in
        let mds = read_file (module Fp) mds_poseidon128_filename nb_mds in
        let mds =
          Array.init width (fun i ->
              Array.init width (fun j -> mds.((i * width) + j)))
        in
        let constants, unbatched_arc =
          Mec.Permutation.Hades_linear_optimisation.compute_updated_constants
            (module Fp)
            r_p
            r_f
            width
            batch_size
            arc
            mds
        in
        Printf.printf
          "Nb batched coefficients and ark: %d\n"
          (Array.length constants) ;
        Array.iter (fun x -> print_endline @@ Fp.to_string x) constants ;
        print_endline "---------------\n\n" ;
        Printf.printf "Nb unbatched ark: %d\n" (Array.length unbatched_arc) ;
        Array.iter (fun x -> print_endline @@ Fp.to_string x) unbatched_arc ;
        print_endline "---------------\n\n")

let () = Core.Command.run ~version:"1.0" ~build_info:"dune exec" command
