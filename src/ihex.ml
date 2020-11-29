open Core

let calculate_crc hex_str =
  let bin = `Hex hex_str |> Hex.to_bytes in
  let sum = Core.Bytes.fold bin ~init:0 ~f:(fun acc v -> acc + (Char.to_int v)) in
  (((sum land 0xff) lxor 0xff) + 1) land 0xff

let line_to_string address data =
  let addr_type = Printf.sprintf "%02X%04X00" (Bytes.length data) address in
  let hex_data = Hex.of_bytes data in
  let hex_data_str = Hex.show hex_data |> Stdlib.String.uppercase_ascii in
  let line = addr_type ^ hex_data_str in
  ":" ^ line ^ Printf.sprintf "%02X\n" (calculate_crc line)

let rec data_to_string offset i data =
  let n = min 32 ((Bytes.length data) - i) in
  if n > 0 then begin
    let line_data = Bytes.sub data ~pos:i ~len:n in
    let line_str = line_to_string (i + offset) line_data in
    line_str :: data_to_string offset (i + 32) data
  end else
    []

let test_image offset n =
  let b = Bytes.make n '\x56' in
  data_to_string offset 0 b
