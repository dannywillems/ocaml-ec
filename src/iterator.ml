type bit = int

module Bit = struct
  type t = unit -> bit option

  let of_bytes_le b =
    let length = Bytes.length b in
    if length = 0 then fun () -> None
    else
      let current_byte_int = ref (Bytes.get_uint8 b 0) in
      let i = ref 0 in
      let j = ref 0 in
      let rec f () : bit option =
        if !i = length - 1 && !j = 8 then None
        else if !j = 8 then (
          i := !i + 1 ;
          current_byte_int := Bytes.get_uint8 b !i ;
          j := 0 ;
          f () )
        else
          let b = !current_byte_int mod 2 in
          j := !j + 1 ;
          current_byte_int := !current_byte_int lsr 1 ;
          Some b
      in
      f

  let create_from_bool_list bs =
    let bs_ref = ref bs in
    let f () =
      match !bs_ref with
      | [] -> None
      | b :: bs ->
          bs_ref := bs ;
          Some (if b then 1 else 0)
    in
    f

  let next i = i ()
end
