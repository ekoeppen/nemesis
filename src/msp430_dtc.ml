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

let add_header (word : Ast1.definition) data base latest =
  align data;
  let l = Data.here data in
  append_address (if latest <> 0 then (latest + base) else 0) data;
  Buffer.add_char data (if (word.immediate) then '\xfe' else '\xff');
  Buffer.add_char data '\xff';
  Buffer.add_char data (char_of_int (String.length word.name));
  Buffer.add_string data word.name;
  align data;
  word.address <- (Data.here data) + base;
  l

let create_image_data =
  Buffer.create (32 * 1024)

let create_info p data latest latest_ram base info user =
  let turnkey = Ast1.find_word_or_zero p "turnkey" in
  let header = Buffer.create ((5 * 2 + 4 * 1) * 2) in
  let dp = (((Data.here data) + base) + 512) land 0xFE00 in
  Data.append_short turnkey header;
  Data.append_short (latest + base) header;
  Data.append_short dp header;
  Data.append_short latest_ram header;
  Data.append_int user header;
  Data.append_short turnkey header;
  Data.append_short (latest + base) header;
  Data.append_short dp header;
  Data.append_short latest_ram header;
  Data.append_short turnkey header;
  Data.append_int user header;
  Ihex.data_to_string info 0 (Stdlib.Buffer.to_bytes header)

let rec create_vectors p n =
  if n > 31 then []
  else begin
    let handler_addr = (match Ast1.find_word
      p ("vector-" ^ string_of_int n) with
    | Some a -> a.address
    | None -> 0xffff) in
    let str = Printf.sprintf "02FF%0X00%02X%02X"
      (0xc0 + n * 2) (handler_addr land 255) (handler_addr / 256) in
    (":" ^ str ^ Printf.sprintf "%02X\n" (Ihex.calculate_crc str)) ::
      create_vectors p (n + 1)
  end

let finalize_image_data p data latest latest_ram base info user =
  let info_lines = create_info p data latest latest_ram base info user in
  let vector_lines = create_vectors p 0 in
  let body_lines = Ihex.data_to_string base 0 (Stdlib.Buffer.to_bytes data) in
  String.concat (info_lines @ body_lines @ vector_lines @ [":00000001FF\n"])
  |> Bytes.of_string
