module MakeProjectiveWeierstrass
    (Fq : Ff_sig.PRIME)
    (Fp : Ff_sig.PRIME) (Params : sig
      val a : Fq.t

      val b : Fq.t

      val bytes_generator : Bytes.t
    end) :
  Ec_sig.ProjectiveWeierstrassT with type Scalar.t = Fp.t and type Base.t = Fq.t =
struct
  let () = assert (not (Fq.is_zero Params.b))

  exception Not_on_curve of Bytes.t

  module Base = Fq
  module Scalar = Fp

  let a = Params.a

  let b = Params.b

  type t = { x : Fq.t; y : Fq.t; z : Fq.t }

  let size_in_bytes = Fq.size_in_bytes * 3

  let zero = { x = Fq.zero; y = Fq.one; z = Fq.zero }

  let is_zero t = Fq.(t.x = zero) && Fq.(t.z = zero)

  let is_on_curve x y z =
    if Fq.is_zero x && Fq.is_zero z then true
    else if Fq.is_zero z then false
    else
      let x' = Fq.(x / z) in
      let y' = Fq.(y / z) in
      Fq.((x' * x' * x') + (a * x') + b = y' * y')

  let of_bytes_opt bytes =
    (* no need to copy the bytes [p] because [Bytes.sub] is used and [Bytes.sub]
       creates a new buffer *)
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
            let x' = Fq.(x / z) in
            let y' = Fq.(y / z) in
            if Fq.((x' * x' * x') + (a * x') + b = y' * y') then
              Some { x; y; z }
            else None

  let check_bytes bytes =
    match of_bytes_opt bytes with Some _ -> true | None -> false

  let of_bytes_exn b =
    (* no need to copy the bytes [p] because [Bytes.sub] is used and [Bytes.sub]
       creates a new buffer *)
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
    if is_zero t1 then t2
    else if is_zero t2 then t1
    else
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

  let mul x n =
    let rec aux x n =
      let two_z = Z.succ Z.one in
      if Z.equal n Z.zero then zero
      else if Z.equal n Z.one then x
      else
        let (a, r) = Z.ediv_rem n two_z in
        if Z.equal r Z.zero then aux (double x) a else add x (aux x (Z.pred n))
    in
    aux x (Scalar.to_z n)

  let get_x_coordinate t = t.x

  let get_y_coordinate t = t.y

  let get_z_coordinate t = t.z

  let from_coordinates_exn ~x ~y ~z =
    if is_on_curve x y z then { x; y; z }
    else
      raise
        (Not_on_curve
           (Bytes.concat
              Bytes.empty
              [Fq.to_bytes x; Fq.to_bytes y; Fq.to_bytes z]))

  let from_coordinates_opt ~x ~y ~z =
    if is_on_curve x y z then Some { x; y; z } else None

  let get_affine_x_coordinate t =
    if is_zero t then failwith "Zero" else Fq.(t.x / t.z)

  let get_affine_y_coordinate t =
    if is_zero t then failwith "Zero" else Fq.(t.y / t.z)

  let from_affine_coordinates_exn ~x ~y = from_coordinates_exn ~x ~y ~z:Fq.one

  let from_affine_coordinates_opt ~x ~y = from_coordinates_exn ~x ~y ~z:Fq.one
end

module MakeAffineEdwards
    (Base : Ff_sig.PRIME)
    (Scalar : Ff_sig.PRIME) (Params : sig
      val a : Base.t

      val d : Base.t

      val cofactor : Z.t

      val bytes_generator : Bytes.t
    end) :
  Ec_sig.AffineEdwardsT with type Base.t = Base.t and type Scalar.t = Scalar.t =
struct
  (* https://www.hyperelliptic.org/EFD/g1p/auto-twisted.html *)
  (* https://en.wikipedia.org/wiki/Twisted_Edwards_curve *)
  exception Not_on_curve of Bytes.t

  module Scalar = Scalar
  module Base = Base
  include Params

  let () =
    (* Addition formula is complete if d is a non square and if a is a square *)
    assert (Option.is_none (Base.sqrt_opt d)) ;
    assert (Option.is_some (Base.sqrt_opt a))

  let size_in_bytes = Base.size_in_bytes * 2

  type t = { u : Base.t; v : Base.t }

  let zero = { u = Base.zero; v = Base.one }

  let is_zero { u; v } = Base.(u = zero) && Base.(v = one)

  let add { u = u1; v = v1 } { u = u2; v = v2 } =
    let u1v2 = Base.(u1 * v2) in
    let v1u2 = Base.(v1 * u2) in
    let u1u2v1v2 = Base.(u1v2 * v1u2) in
    let v1v2 = Base.(v1 * v2) in
    let u1u2 = Base.(u1 * u2) in
    let du1u2v1v2 = Base.(d * u1u2v1v2) in
    let u = Base.((u1v2 + v1u2) / (Base.one + du1u2v1v2)) in
    let v =
      Base.(
        (v1v2 + Base.negate (a * u1u2)) / (Base.one + Base.negate du1u2v1v2))
    in
    { u; v }

  let double { u; v } =
    let uv = Base.(u * v) in
    let uu = Base.square u in
    let vv = Base.square v in
    let neg_uu = Base.negate uu in
    let neg_vv = Base.negate vv in
    (* a u^2 v^2 = 1 + d u^2 v^2 --> we can skip one multiplication *)
    let u' = Base.(double uv / ((a * uu) + vv)) in
    let v' = Base.((vv + (a * neg_uu)) / (one + one + (a * neg_uu) + neg_vv)) in
    { u = u'; v = v' }

  let negate { u; v } = { u = Base.negate u; v }

  let eq { u = u1; v = v1 } { u = u2; v = v2 } = Base.(u1 = u2 && v1 = v2)

  let mul x n =
    let rec aux x n =
      let two_z = Z.succ Z.one in
      if Z.equal n Z.zero then zero
      else if Z.equal n Z.one then x
      else
        let (q, r) = Z.ediv_rem n two_z in
        let x_plus_x = double x in
        if Z.equal r Z.zero then aux x_plus_x q else add x (aux x_plus_x q)
    in
    aux x (Scalar.to_z n)

  let is_small_order p = eq (mul p (Scalar.of_z cofactor)) zero

  let is_torsion_free p = eq (mul p Scalar.(of_z order)) p

  let is_prime_order p = is_torsion_free p && not (is_zero p)

  let is_on_curve u v =
    (* a * u^2 + v^2 = 1 + d u^2 v^2 *)
    let uu = Base.square u in
    let vv = Base.square v in
    let uuvv = Base.(uu * vv) in
    Base.((a * uu) + vv = one + (d * uuvv))

  let of_bytes_opt b =
    (* no need to copy the bytes [p] because [Bytes.sub] is used and [Bytes.sub]
       creates a new buffer *)
    if Bytes.length b <> size_in_bytes then None
    else
      let u_opt = Base.of_bytes_opt (Bytes.sub b 0 Base.size_in_bytes) in
      let v_opt =
        Base.of_bytes_opt (Bytes.sub b Base.size_in_bytes Base.size_in_bytes)
      in
      match (u_opt, v_opt) with
      | (Some u, Some v) -> if is_on_curve u v then Some { u; v } else None
      | _ -> None

  let of_bytes_exn b =
    (* no need to copy the bytes [p] because [Bytes.sub] is used in
       [of_bytes_opt] and [Bytes.sub] creates a new buffer *)
    match of_bytes_opt b with None -> raise (Not_on_curve b) | Some p -> p

  let check_bytes b = match of_bytes_opt b with None -> false | Some _ -> true

  let to_bytes { u; v } =
    Bytes.concat Bytes.empty [Base.to_bytes u; Base.to_bytes v]

  let one = of_bytes_exn bytes_generator

  let rec random ?state () =
    let () = match state with Some s -> Random.set_state s | None -> () in
    let u = Base.random ?state:None () in
    let uu = Base.(square u) in
    let auu = Base.(a * uu) in
    let duu = Base.(d * uu) in
    if Base.(is_one duu) then random ?state:None ()
    else
      (*      a u^2 + v^2 = 1 + d u^2 v^2 *)
      (* <==> a u^2 + v^2 - d u^2 v^2 = 1 *)
      (* <==> v^2 - d u^2 v^2 = 1 - a u^2 *)
      (* <==> v^2 * (1 - d u^2) = 1 - a u^2 *)
      (* <==> v^2 = (1 - a * u^2) / (1 - d * u^2) *)
      let tmp = Base.((one + negate auu) / (one + negate duu)) in
      let v_sqrt = Base.(sqrt_opt tmp) in
      match v_sqrt with
      | None -> random ?state:None ()
      | Some v ->
          let p = mul { u; v } (Scalar.of_z cofactor) in
          if eq p zero then random ?state:None () else p

  let get_u_coordinate p = p.u

  let get_v_coordinate p = p.v

  let from_coordinates_opt ~u ~v =
    let p = { u; v } in
    if is_on_curve u v then Some p else None

  let from_coordinates_exn ~u ~v =
    match from_coordinates_opt ~u ~v with
    | None ->
        raise
          (Not_on_curve
             (Bytes.concat Bytes.empty [Base.to_bytes u; Base.to_bytes v]))
    | Some p -> p

  let unsafe_from_coordinates ~u ~v = { u; v }
end
