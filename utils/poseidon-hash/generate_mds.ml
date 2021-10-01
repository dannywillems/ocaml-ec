let generate_mds (type a) (module Fp : Ff_sig.PRIME with type t = a) width =
  let matrix = Array.make_matrix width width Fp.zero in
  let xs = Array.init width (fun i -> Fp.of_z (Z.of_int i)) in
  let ys = Array.init width (fun i -> Fp.of_z (Z.of_int (width + i))) in
  Array.iteri
    (fun i _ ->
      Array.iteri
        (fun j _ -> matrix.(i).(j) <- Fp.(inverse_exn (xs.(i) + ys.(j))))
        matrix.(i))
    matrix ;
  matrix

let command =
  Core.Command.basic
    ~summary:"Generate a set of constant"
    ~readme:(fun () -> "TODO")
    Core.Command.Let_syntax.(
      let%map_open width = anon ("width" %: int)
      and field_size = anon ("field-size" %: string) in
      fun () ->
        let field_size = Z.of_string field_size in
        let module Fp = Ff.MakeFp (struct
          let prime_order = field_size
        end) in
        let res = generate_mds (module Fp) width in
        Array.iter
          (fun l ->
            Printf.printf
              "%s\n"
              (String.concat " " (List.map Fp.to_string (Array.to_list l))))
          res ;
        ())

let () = Core.Command.run ~version:"1.0" ~build_info:"dune exec" command
