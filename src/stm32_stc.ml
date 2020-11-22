open Core

let cell_size = 4

let align data =
  Data.align cell_size data

let add_enter data =
  Buffer.add_char data '\x00';
  Buffer.add_char data '\xb5'

let add_exit data =
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

let append_inline_constant data =
  List.iter ~f:(fun c -> Data.append_short c data)
    [0x3E04; 0x6030; 0x4800; 0x46F7]

let append_call addr data =
  Data.append_int (addr_to_branch addr data) data

let append_address addr data =
  Data.append_int addr data

let append_number n data =
  Data.append_int n data

let append_code code data =
  Data.append_short code data
