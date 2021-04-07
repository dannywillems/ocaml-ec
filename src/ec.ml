module MakeProjectiveWeierstrass
    (Fq : Ff_sig.PRIME)
    (Fp : Ff_sig.PRIME) (Params : sig
      val a : Fq.t

      val b : Fq.t

      val bytes_generator : Bytes.t
    end) :
  Ec_sig.ProjectiveWeierstrassT
    with type ScalarField.t = Fp.t
     and type BaseField.t = Fq.t = struct
  let () = assert (not (Fq.is_zero Params.b))

  exception Not_on_curve of Bytes.t

  module BaseField = Fq
  module ScalarField = Fp

  let a = Params.a

  let b = Params.b

  type t = { x : Fq.t; y : Fq.t; z : Fq.t }

  let size_in_bytes = Fq.size_in_bytes * 3

  let zero = { x = Fq.zero; y = Fq.one; z = Fq.zero }

  let is_zero t = Fq.(t.x = zero) && Fq.(t.z = zero)

  let of_bytes_opt bytes =
    if Bytes.length bytes <> size_in_bytes then None
    else
      let x_bytes = Bytes.sub bytes 0 Fq.size_in_bytes in
      let y_bytes = Bytes.sub bytes Fq.size_in_bytes Fq.size_in_bytes in
      let z_bytes = Bytes.sub bytes (2 * Fq.size_in_bytes) Fq.size_in_bytes in
      let x = Fq.of_bytes_opt x_bytes in
      let y = Fq.of_bytes_opt y_bytes in
      let z = Fq.of_bytes_opt z_bytes in
      match (x, y, z) with
      | (None, _, _) | (_, None, _) | (_, _, None) -> None
      (* Verify it is on the curve *)
      | (Some x, Some y, Some z) ->
          if Fq.is_zero x && Fq.is_zero z then Some zero
          else if Fq.is_zero z then None
          else
            let x = Fq.(x / z) in
            let y = Fq.(y / z) in
            if Fq.((x * x * x) + (a * x) + b = y * y) then Some { x; y; z }
            else None

  let check_bytes bytes =
    match of_bytes_opt bytes with Some _ -> true | None -> false

  let of_bytes_exn b =
    match of_bytes_opt b with Some g -> g | None -> raise (Not_on_curve b)

  let to_bytes g =
    let buffer = Bytes.make size_in_bytes '\000' in
    Bytes.blit (Fq.to_bytes g.x) 0 buffer 0 Fq.size_in_bytes ;
    Bytes.blit (Fq.to_bytes g.y) 0 buffer Fq.size_in_bytes Fq.size_in_bytes ;
    Bytes.blit
      (Fq.to_bytes g.z)
      0
      buffer
      (2 * Fq.size_in_bytes)
      Fq.size_in_bytes ;
    buffer

  let one = of_bytes_exn Params.bytes_generator

  let random ?state () =
    (match state with None -> () | Some s -> Random.set_state s) ;
    let rec aux () =
      let x = Fq.random () in
      let y_square = Fq.((x * x * x) + (a * x) + b) in
      let y_opt = Fq.sqrt_opt y_square in
      match y_opt with None -> aux () | Some y -> { x; y; z = Fq.one }
    in
    aux ()

  let add t1 t2 =
    (* See https://github.com/o1-labs/snarky/blob/master/snarkette/elliptic_curve.ml *)
    let open Fq in
    let x1z2 = t1.x * t2.z in
    let x2z1 = t1.z * t2.x in
    let y1z2 = t1.y * t2.z in
    let y2z1 = t1.z * t2.y in
    if x1z2 = x2z1 && y1z2 = y2z1 then
      (* Double case *)
      let xx = square t1.x in
      let zz = square t1.z in
      let w = (a * zz) + (xx + xx + xx) in
      let y1z1 = t1.y * t1.z in
      let s = y1z1 + y1z1 in
      let ss = square s in
      let sss = s * ss in
      let r = t1.y * s in
      let rr = square r in
      let b = square (t1.x + r) + negate xx + negate rr in
      let h = square w + negate (b + b) in
      let x3 = h * s in
      let y3 = (w * (b + negate h)) + negate (rr + rr) in
      let z3 = sss in
      { x = x3; y = y3; z = z3 }
    else
      (* Generic case *)
      let z1z2 = t1.z * t2.z in
      let u = y2z1 + negate y1z2 in
      let uu = square u in
      let v = x2z1 + negate x1z2 in
      let vv = square v in
      let vvv = v * vv in
      let r = vv * x1z2 in
      let a = (uu * z1z2) + negate (vvv + r + r) in
      let x3 = v * a in
      let y3 = (u * (r + negate a)) + negate (vvv * y1z2) in
      let z3 = vvv * z1z2 in
      { x = x3; y = y3; z = z3 }

  let double t = add t t

  let negate { x; y; z } = { x; y = Fq.negate y; z }

  let eq t1 t2 =
    if Fq.(is_zero t1.z) && Fq.(is_zero t2.z) then true
    else if Fq.is_zero t1.z || Fq.is_zero t2.z then false
    else
      let x1 = Fq.(t1.x / t1.z) in
      let x2 = Fq.(t2.x / t2.z) in
      let y1 = Fq.(t1.y / t1.z) in
      let y2 = Fq.(t2.y / t2.z) in
      Fq.(x1 = x2 && y1 = y2)

  let two_z = Z.succ Z.one

  let mul x n =
    let rec aux x n =
      if Z.equal n Z.one then x
      else
        let (a, r) = Z.ediv_rem n two_z in
        let acc = aux x a in
        let acc_add = add acc acc in
        if Z.equal r Z.zero then acc_add else add acc_add x
    in
    let n = ScalarField.to_z n in
    if Z.equal n Z.zero then zero else if is_zero x then zero else aux x n

  let get_x_coordinate t = t.x

  let get_y_coordinate t = t.y

  let get_z_coordinate t = t.z

  let from_coordinates_exn ~x ~y ~z = { x; y; z }

  let from_coordinates_opt ~x ~y ~z = Some { x; y; z }
end

module MakeTwistedEdwards
    (Base : Ff_sig.PRIME)
    (Scalar : Ff_sig.PRIME) (Params : sig
      val a : Base.t

      val d : Base.t

      val bytes_generator : Bytes.t
    end) : Ec_sig.TwistedEdwardsT = struct
  (* https://www.hyperelliptic.org/EFD/g1p/auto-twisted.html *)
  (* https://en.wikipedia.org/wiki/Twisted_Edwards_curve *)
  exception Not_on_curve of Bytes.t

  module ScalarField = Scalar
  module BaseField = Base
  include Params

  let () =
    (* Addition formula is complete if d is a non square and if a is a square *)
    assert (Option.is_none (BaseField.sqrt_opt d)) ;
    assert (Option.is_some (BaseField.sqrt_opt a))

  let size_in_bytes = Base.size_in_bytes * 2

  type t = { u : Base.t; v : Base.t }

  let is_on_curve u v =
    let uu = Base.square u in
    let vv = Base.square v in
    let uuvv = Base.(uu * vv) in
    Base.((a * uu) + vv = one + (d * uuvv))

  let check_bytes b =
    if Bytes.length b != size_in_bytes then false
    else
      let u_opt = Base.of_bytes_opt (Bytes.sub b 0 Base.size_in_bytes) in
      let v_opt =
        Base.of_bytes_opt (Bytes.sub b Base.size_in_bytes Base.size_in_bytes)
      in
      match (u_opt, v_opt) with
      | (Some u, Some v) -> is_on_curve u v
      | _ -> false

  let of_bytes_opt b =
    if Bytes.length b != size_in_bytes then raise (Not_on_curve b)
    else
      let u_opt = Base.of_bytes_opt (Bytes.sub b 0 Base.size_in_bytes) in
      let v_opt =
        Base.of_bytes_opt (Bytes.sub b Base.size_in_bytes Base.size_in_bytes)
      in
      match (u_opt, v_opt) with
      | (Some u, Some v) -> if is_on_curve u v then Some { u; v } else None
      | _ -> None

  let of_bytes_exn b =
    match of_bytes_opt b with None -> raise (Not_on_curve b) | Some p -> p

  let to_bytes { u; v } =
    Bytes.concat Bytes.empty [Base.to_bytes u; Base.to_bytes v]

  let zero = { u = BaseField.zero; v = BaseField.one }

  let one = of_bytes_exn bytes_generator

  let is_zero { u; v } = Base.(u = zero) && Base.(v = one)

  let add { u = x1; v = y1 } { u = x2; v = y2 } =
    let x1y2 = Base.(x1 * y2) in
    let y1x2 = Base.(y1 * x2) in
    let x1x2y1y2 = Base.(x1y2 * y1x2) in
    let y1y2 = Base.(y1 * y2) in
    let x1x2 = Base.(x1 * x2) in
    let u = Base.((x1y2 + y1x2) / (Base.one + (d * x1x2y1y2))) in
    let v =
      Base.(
        (y1y2 + Base.negate (a * x1x2)) / (Base.one + Base.negate (d * x1x2y1y2)))
    in
    { u; v }

  let double p = add p p

  (* let u_plus_v = Base.(u + v) in
   * let u_plus_v_square = Base.square u_plus_v in
   * let uu = Base.square u in
   * let vv = Base.square v in
   * let neg_uu = Base.negate uu in
   * let neg_vv = Base.negate vv in
   * let u' = Base.((u_plus_v_square + neg_uu + neg_vv) / (uu + vv)) in
   * let v' = Base.((vv + neg_uu) / (Base.(one + one) + neg_uu + neg_vv)) in
   * { u = u'; v = v' } *)

  let rec random ?state () =
    let u = Base.random ?state () in
    let uu = Base.(double u) in
    let auu = Base.(a * uu) in
    let duu = Base.(d * uu) in
    if Base.(is_one duu) then random ?state ()
    else
      (* y^2 = (1 - a * x^2) / (1 - d * x ^ 2) *)
      let tmp = Base.((one + negate auu) / (one + negate duu)) in
      let v_sqrt = Base.(sqrt_opt tmp) in
      match v_sqrt with None -> random ?state () | Some v -> { u; v }

  let negate { u; v } = { u = Base.negate u; v }

  let eq { u = u1; v = v1 } { u = u2; v = v2 } = BaseField.(u1 = u2 && v1 = v2)

  let mul x n =
    let two_z = Z.succ Z.one in
    let rec aux x n =
      if Z.equal n Z.one then x
      else
        let (a, r) = Z.ediv_rem n two_z in
        let acc = aux x a in
        let acc_add = double acc in
        if Z.equal r Z.zero then acc_add else add acc_add x
    in
    let n = ScalarField.to_z n in
    if Z.equal n Z.zero then zero else if is_zero x then zero else aux x n

  let get_u_coordinate p = p.u

  let get_v_coordinate p = p.v

  let from_coordinates_opt ~u ~v =
    let uu = Base.square u in
    let vv = Base.square v in
    let uuvv = Base.(uu * vv) in
    if Base.((a * uu) + vv = one + (d * uuvv)) then Some { u; v } else None

  let from_coordinates_exn ~u ~v =
    match from_coordinates_opt ~u ~v with
    | None ->
        raise
          (Not_on_curve
             (Bytes.concat Bytes.empty [Base.to_bytes u; Base.to_bytes v]))
    | Some p -> p
end
