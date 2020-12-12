open Core

let cell_size: int = 4
let stack = 0x20004000

let align data =
  Data.align cell_size data

let add_enter _docol data =
  Buffer.add_char data '\x00';
  Buffer.add_char data '\xb5'

let add_exit _exit data =
  Buffer.add_char data '\x00';
  Buffer.add_char data '\xbd'

let addr_to_branch addr data =
  let delta = (addr - (Data.here data) - cell_size) asr 1 in
  let lower = (delta asr 11) land 0x000003ff in
  let upper = (delta lsl 16) land 0x7fff0000 in
  lower lor upper lor 0xf800f400

let append_placeholder_address data =
  Data.append_int 0xffffffff data

let update_address addr n data =
  Data.update_int addr n data

let append_inline_constant n data =
  List.iter ~f:(fun c -> Data.append_short c data)
    [0x3E04; 0x6030; 0x4800; 0x46F7];
  Data.append_int n data

let append_call addr data =
  Data.append_int (addr_to_branch addr data) data

let append_address addr data =
  Data.append_int addr data

let append_number n data =
  Data.append_int n data

let append_code code data =
  Data.append_short code data

let create_image_data =
  let data = Buffer.create (64 * 1024) in
  Buffer.add_bytes data (Bytes.make 0x90 '\xff');
  data

let finalize_image_data p data latest base _info ram _user =
  let cold = Ast1.find_word_or_zero p "cold" in
  let reset_handler = Ast1.find_word_or_zero p "reset-handler" in
  let header = Buffer.create 0x90 in
  Data.append_int stack header;
  Data.append_int (reset_handler + 1) header;
  Buffer.add_bytes header (Bytes.make (0x90 - 4 - 4 - (4 * 4)) '\xff');
  Data.append_int (cold + 1) header;
  Data.append_int (latest + base) header;
  Data.append_int ((Data.here data) + base) header;
  Data.append_int ram header;
  let final = Bytes.create (Data.here data) in
  Buffer.blit ~src:header ~src_pos:0 ~dst:final ~dst_pos:0 ~len:0x90;
  Buffer.blit ~src:data ~src_pos:0x90 ~dst:final
    ~dst_pos:0x90 ~len:((Data.here data) - 0x90);
  final
