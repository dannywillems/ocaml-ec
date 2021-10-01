let ifloor x = int_of_float (floor (float_of_int x))

let iceil x = int_of_float (ceil (float_of_int x))

let fmin (x : int) (y : int) = float_of_int (min x y)

let flog_b_x b x = log x /. log b

let max_l l = List.fold_left max 0. l

let zip l1 l2 =
  let l = List.map (fun x1 -> List.map (fun x2 -> (x1, x2)) l2) l1 in
  List.flatten l

(* Return the number of sbox. This is the cost function used to get the minimal
   parameter, i.e. we want to lower the number of s_box
*)
let compute_cost r_f r_p width = (width * r_f) + r_p

(* The equations are based on the details given in the paper
   https://eprint.iacr.org/2019/458.pdf, Attack Details. The idea is to brute force
   and take the smalleest values of r_f and r_p which verifies the equations in
   section 5.5
*)
let check_round_values_security (p : Z.t) (width : int) (r_f : int) (r_p : int)
    (alpha : int) (security_level : int) =
  let n = Z.log2 p in
  (* FIXME: compute log2 p in float. Here it is the floor value !! *)
  let f_n = float_of_int (Z.log2 p) in
  let f_alpha = float_of_int alpha in
  let f_width = float_of_int width in
  if alpha > 0 then
    let c = Z.log2 (Z.of_int (alpha - 1)) in
    let cond_right = (n - c) * (width + 1) in
    (* Statistical attack, 5.5.1 *)
    let r_f_statistical = if security_level <= cond_right then 6. else 10. in
    (* Interpolation attack, 5.5.2. Substracting r_p because we have R = r_f +
       r_p and we want a condition on r_f
    *)
    let r_f_interpolation =
      ceil (flog_b_x f_alpha 2. *. fmin security_level n)
      +. ceil (flog_b_x f_alpha f_width)
      -. float_of_int r_p
    in
    (* Groebner 1 attack, 5.5.3. Substracting r_p because we have R = r_f +
       r_p and we want a condition on r_f
    *)
    let r_f_groebner_1 =
      flog_b_x f_alpha 2.
      *. min (float_of_int security_level /. 3.) (f_n /. 2.)
      -. float_of_int r_p
    in
    let r_f_groebner_2 =
      float_of_int (width - 1 - r_p)
      +.
      let v1 =
        float_of_int security_level
        *. flog_b_x f_alpha 2.
        /. float_of_int (width + 1)
      in
      (* FIXME: Z.log2 p is not correct because we might have some decimals. Can give wrong values!!! *)
      let v2 = flog_b_x f_alpha 2. *. (float_of_int @@ Z.log2up p) in
      min v1 v2
    in
    (* Printf.printf "R_f_statistical = %f\n" r_f_statistical ; *)
    (* Printf.printf "R_f_interpolation = %f\n" r_f_interpolation ; *)
    (* Printf.printf "R_f_groebner 1 = %f\n" r_f_groebner_1 ; *)
    (* Printf.printf "R_f_groebner 2 = %f\n" r_f_groebner_2 ; *)
    let max_computed_f =
      max_l [r_f_statistical; r_f_interpolation; r_f_groebner_1; r_f_groebner_2]
    in
    (* if float_of_int r_f >= max_computed_f then *)
    (*   Printf.printf *)
    (*     "Mx computed = %f (r_f = %d) cost = %d\n" *)
    (*     max_computed_f *)
    (*     r_f *)
    (*     (compute_cost r_f r_p width) ; *)
    float_of_int r_f >= max_computed_f
  else failwith "Not supported yet"

let compute_number_of_rounds p width alpha security_level =
  let values =
    zip (List.init 48 (fun i -> 4 + (i * 2))) (List.init 500 (fun i -> i + 1))
  in
  let l =
    List.map
      (fun (r_f, r_p) ->
        ( check_round_values_security p width r_f r_p alpha security_level,
          compute_cost r_f r_p width,
          r_f,
          r_p ))
      values
  in
  let l = List.filter (fun (b, _, _, _) -> b) l in
  let (values_with_minimum_cost, cost) =
    List.fold_left
      (fun (acc, min_cost) (_, cost, r_f, r_p) ->
        if cost > min_cost then (acc, min_cost)
        else if cost = min_cost then ((r_f, r_p) :: acc, min_cost)
        else ([(r_f, r_p)], cost))
      ([], max_int)
      l
  in
  (values_with_minimum_cost, cost)

let command =
  Core.Command.basic
    ~summary:"Compute number of partial rounds and full rounds for Poseidon"
    ~readme:(fun () -> "TODO")
    Core.Command.Let_syntax.(
      let%map_open security_level = anon ("security_level" %: int)
      and alpha = anon ("alpha" %: int)
      and width = anon ("width" %: int)
      and field_size = anon ("field-size" %: string) in
      fun () ->
        let field_size = Z.of_string field_size in
        let (values, cost) =
          compute_number_of_rounds field_size width alpha security_level
        in
        Printf.printf "Found values with cost %d:\n" cost ;
        List.iter
          (fun (r_f, r_p) ->
            Printf.printf
              "R_f = %d, R_p = %d. With (arbitrary) additional security \
               consideration: R_f %d, R_p %d\n"
              r_f
              r_p
              (r_f + 2)
              (int_of_float @@ ceil @@ (float_of_int r_p *. 1.075)))
          values)

let () = Core.Command.run ~version:"1.0" ~build_info:"RWO" command
