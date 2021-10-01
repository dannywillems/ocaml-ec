let () = Random.self_init ()

let () =
  let r = Mec.Curve.Pallas.Affine.(to_bytes (random ())) in
  Js_of_ocaml.Firebug.console##log
    (Js_of_ocaml.Js.string Hex.(show (of_bytes r)))
