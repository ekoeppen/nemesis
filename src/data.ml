open Core

let align a data =
  let n = (((Buffer.length data) + (a - 1)) land (lnot (a - 1))) in
  let delta = n - Buffer.length data in
  if delta > 0 then Buffer.add_bytes data (Bytes.make delta '\xff')

let append_short n data =
  let q = (if n < 0 then 0xffffffff + n + 1 else n) in
  Buffer.add_char data (char_of_int (q mod 256));
  Buffer.add_char data (char_of_int ((q / 256) mod 256))

let append_int n data =
  let q = (if n < 0 then 0xffffffff + n + 1 else n) in
  Buffer.add_char data (char_of_int (q mod 256));
  Buffer.add_char data (char_of_int ((q / 256) mod 256));
  Buffer.add_char data (char_of_int ((q / 256 / 256) mod 256));
  Buffer.add_char data (char_of_int ((q / 256 / 256 / 256 ) mod 256))

let here =
  Buffer.length

let update_char (addr: int) (c: char) (data: Buffer.t) =
  let head_length = addr in
  let tail_length = (Buffer.length data) - head_length - 1 in
  let temp_head = Bytes.create head_length in
  let temp_tail = Bytes.create tail_length in
  Buffer.blit ~src:data ~src_pos:0 ~dst:temp_head ~dst_pos:0 ~len:head_length;
  Buffer.blit ~src:data ~src_pos:(head_length + 1) ~dst:temp_tail
    ~dst_pos:0 ~len:tail_length;
  Buffer.clear data;
  Buffer.add_bytes data temp_head;
  Buffer.add_char data c;
  Buffer.add_bytes data temp_tail

let update_int (addr: int) (n: int) (data: Buffer.t) =
  let head_length = addr in
  let tail_length = (here data) - head_length - 4 in
  let temp_head = Bytes.create head_length in
  let temp_tail = Bytes.create tail_length in
  Buffer.blit ~src:data ~src_pos:0 ~dst:temp_head ~dst_pos:0 ~len:head_length;
  Buffer.blit ~src:data ~src_pos:(head_length + 4) ~dst:temp_tail
    ~dst_pos:0 ~len:tail_length;
  Buffer.clear data;
  Buffer.add_bytes data temp_head;
  append_int n data;
  Buffer.add_bytes data temp_tail
