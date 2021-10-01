let sha256 bs = Hex.to_bytes (`Hex Digestif.SHA256.(to_hex (digest_bytes bs)))

let generate_ark (type a) (module Fp : Ff_sig.PRIME with type t = a) r_f r_p
    width seed =
  let nb_constant = (r_f + r_p) * width in
  let rec inner i acc last_elmt bs =
    if i = nb_constant then acc
    else
      let bs = sha256 bs in
      let res = Fp.of_bytes_exn bs in
      let x = Fp.(res + last_elmt) in
      let acc = x :: acc in
      inner (i + 1) acc x bs
  in
  inner 0 [] Fp.one seed

let command =
  Core.Command.basic
    ~summary:"Generate a set of constant"
    ~readme:(fun () -> "TODO")
    Core.Command.Let_syntax.(
      let%map_open r_f = anon ("r_f" %: int)
      and r_p = anon ("r_p" %: int)
      and width = anon ("width" %: int)
      and field_size = anon ("field-size" %: string)
      and seed = anon ("seed" %: string) in
      fun () ->
        let field_size = Z.of_string field_size in
        let module Fp = Ff.MakeFp (struct
          let prime_order = field_size
        end) in
        let res =
          generate_ark (module Fp) r_f r_p width (Bytes.of_string seed)
        in
        List.iter (fun x -> Printf.printf "%s\n" (Fp.to_string x)) res)

let () = Core.Command.run ~version:"1.0" ~build_info:"dune exec" command
