open Core

let cell_size: int = 2
let base: int = 0xc000

let ram = 0x0296
let stack = 0x0380

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
  let data = Buffer.create (16 * 1024) in
  Buffer.add_bytes data (Bytes.make 0x10 '\xff');
  data

let create_header p data latest =
  let cold = Ast1.find_word_or_zero p "cold" in
  let header = Buffer.create 0x10 in
  Data.append_short cold header;
  Data.append_short (latest + base) header;
  Data.append_short ((Data.here data) + base) header;
  Data.append_short ram header;
  Data.append_int 0 header;
  Data.append_int 0 header;
  header

let finalize_image_data p data latest =
  let header = create_header p data latest in
  let final = Bytes.create (Data.here data) in
  Buffer.blit ~src:header ~src_pos:0 ~dst:final ~dst_pos:0 ~len:0x10;
  Buffer.blit ~src:data ~src_pos:0x10 ~dst:final
    ~dst_pos:0x10 ~len:((Data.here data) - 0x10);
  let reset_handler = Ast1.find_word_or_zero p "reset-handler" in
  let reset_handler_str = Printf.sprintf "02FFFE00%02X%02X"
    (reset_handler land 255)
    (reset_handler / 256) in
  let body_lines = Ihex.data_to_string 0xc000 0 final in
  let vector_lines = [":" ^ reset_handler_str ^
    Printf.sprintf "%02X\n" (Ihex.calculate_crc reset_handler_str)] in
  String.concat (body_lines @ vector_lines @ [":00000001FF\n"])
  |> Bytes.of_string
