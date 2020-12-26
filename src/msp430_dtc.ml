open Core

let cell_size = 2

let align data =
  Data.align cell_size data

let add_enter docol data =
  Data.append_short 0x12b0 data;
  Data.append_short docol data

let add_exit exit data =
  Data.append_short exit data

let append_placeholder_address data =
  Data.append_short 0xffff data

let update_address addr n data =
  Data.update_short addr n data

let append_inline_constant n data =
  List.iter ~f:(fun c -> Data.append_short c data)
    [0x8324; 0x4784; 0x0000; 0x4037; n; 0x4530]

let append_call addr data =
  Data.append_short addr data

let append_address addr data =
  Data.append_short addr data

let append_number n data =
  Data.append_short n data

let append_code code data =
  Data.append_short code data

let create_image_data =
  Buffer.create (32 * 1024)

let create_info p data latest base info ram user =
  let turnkey = Ast1.find_word_or_zero p "turnkey" in
  let header = Buffer.create ((5 * 2 + 4 * 1) * 2) in
  let dp = (((Data.here data) + base) + 512) land 0xFE00 in
  Data.append_short turnkey header;
  Data.append_short (latest + base) header;
  Data.append_short dp header;
  Data.append_short ram header;
  Data.append_int user header;
  Data.append_short turnkey header;
  Data.append_short (latest + base) header;
  Data.append_short dp header;
  Data.append_short ram header;
  Data.append_short turnkey header;
  Data.append_int user header;
  Ihex.data_to_string info 0 (Stdlib.Buffer.to_bytes header)

let rec default_vector_list n addr =
  if n > 30 then []
  else begin
    let str = Printf.sprintf "02FF%0X00%02X%02X"
      (0xc0 + n * 2) (addr land 255) (addr / 256) in
    (":" ^ str ^ Printf.sprintf "%02X\n" (Ihex.calculate_crc str)) ::
      default_vector_list (n + 1) addr
  end

let create_vectors p =
  let default_handler = Ast1.find_word_or_zero p "default-handler" in
  let reset_handler = Ast1.find_word_or_zero p "reset-handler" in
  let reset_handler_str = Printf.sprintf "02FFFE00%02X%02X"
    (reset_handler land 255)
    (reset_handler / 256) in
  (default_vector_list 0 default_handler) @ [":" ^ reset_handler_str ^
    Printf.sprintf "%02X\n" (Ihex.calculate_crc reset_handler_str)]

let patch_reset_handler p data base =
  let reset_handler = Ast1.find_word_or_zero p "reset-handler" in
  let cold = Ast1.find_word_or_zero p "cold" in
  Data.update_short (reset_handler + 12 - base) cold data

let finalize_image_data p data latest base info ram user =
  let () = patch_reset_handler p data base in
  let info_lines = create_info p data latest base info ram user in
  let vector_lines = create_vectors p in
  let body_lines = Ihex.data_to_string base 0 (Stdlib.Buffer.to_bytes data) in
  String.concat (info_lines @ body_lines @ vector_lines @ [":00000001FF\n"])
  |> Bytes.of_string
