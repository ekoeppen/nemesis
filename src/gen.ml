open Ast1
open Core

let latest = ref 0
let base = 0x08000000
let ram = 0x20000080
let stack = 0x20004000

let here_stack = Stack.create ()

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

let push_here n =
  Stack.push here_stack n

let pop_here () =
  Stack.pop_exn here_stack

let here =
  Buffer.length

let update_char addr c data =
  let head_length = addr in
  let tail_length = (here data) - head_length - 1 in
  let temp_head = Bytes.create head_length in
  let temp_tail = Bytes.create tail_length in
  Buffer.blit ~src:data ~src_pos:0 ~dst:temp_head ~dst_pos:0 ~len:head_length;
  Buffer.blit ~src:data ~src_pos:(head_length + 1) ~dst:temp_tail
    ~dst_pos:0 ~len:tail_length;
  Buffer.clear data;
  Buffer.add_bytes data temp_head;
  Buffer.add_char data c;
  Buffer.add_bytes data temp_tail

let update_int addr n data =
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

let add_header (word : Ast1.definition) data =
  align 4 data;
  let l = here data in
  append_int (!latest + base) data;
  latest := l;
  Buffer.add_char data (if (word.immediate) then '\xfe' else '\xff');
  Buffer.add_char data '\xff';
  Buffer.add_char data (char_of_int (String.length word.name));
  Buffer.add_string data word.name;
  align 4 data;
  word.address <- (here data) + base

let add_enter data =
  Buffer.add_char data '\x00';
  Buffer.add_char data '\xb5'

let add_exit data =
  Buffer.add_char data '\x00';
  Buffer.add_char data '\xbd'

let add_deferred data =
  append_short 0x46F7 data;
  append_short 0xffff data;
  append_int 0xffffffff data

let addr_to_branch addr data =
  let delta = (addr - (here data) - 4) asr 1 in
  let lower = (delta asr 11) land 0x000003ff in
  let upper = (delta lsl 16) land 0x7fff0000 in
  lower lor upper lor 0xf800f400

let resolve_word dict word data =
  match Ast1.find_word dict word with
  | Some w ->
      if (w.address = 0)
      then raise (Failure ("Word " ^ w.name ^ " not resolved yet"));
      append_int (addr_to_branch w.address data) data
  | None -> raise (Failure ("Word not found " ^ word))

let compile_word_address dict word data =
  match Ast1.find_word dict word with
  | Some w ->
      if (w.address = 0)
      then raise (Failure ("Word " ^ w.name ^ " not resolved yet"));
      resolve_word dict "lit" data;
      append_int w.address data
  | None -> raise (Failure ("Word not found " ^ word))

let postpone_word dict word data =
  match Ast1.find_word dict word with
  | Some w ->
      if (w.address = 0)
      then raise (Failure ("Word " ^ w.name ^ " not resolved yet"));
      if (w.immediate) then
        begin append_int (addr_to_branch w.address data) data
      end else begin
        resolve_word dict "lit" data;
        append_int w.address data;
        resolve_word dict ",call" data
      end
  | None -> raise (Failure ("Word not found " ^ word))

let append_call dict word data =
  match word with
  | Call s -> resolve_word dict s data
  | String s ->
      resolve_word dict "(s\")" data;
      Buffer.add_char data (char_of_int (String.length s));
      Buffer.add_string data s;
      align 4 data
  | Number n -> resolve_word dict "lit" data; append_int n data
  | Immediate -> update_char (!latest + 4) '\xfe' data
  | If ->
      resolve_word dict "?branch" data;
      push_here (here data);
      append_int 0xffffffff data
  | Else ->
      resolve_word dict "branch" data;
      append_int 0xffffffff data;
      update_int (pop_here ()) ((here data) + base) data;
      push_here ((here data) - 4)
  | Then -> update_int (pop_here ()) ((here data) + base) data
  | Begin -> push_here (here data)
  | Again ->
      resolve_word dict "branch" data;
      append_int (pop_here () + base) data
  | Until ->
      resolve_word dict "?branch" data;
      append_int (pop_here () + base) data
  | While ->
      resolve_word dict "?branch" data;
      push_here (here data); append_int 0xffffffff data
  | Repeat ->
      resolve_word dict "branch" data;
      update_int (pop_here ()) ((here data) + base + 4) data;
      append_int (pop_here () + base) data
  | _ -> ()

let handle_code_word (word : Ast1.definition) data =
  add_header word data;
  let h = here data in
  if (word.constant) then begin
    List.iter ~f:(fun c -> append_short c data)
      [0x3E04; 0x6030; 0x4800; 0x46F7];
    match List.hd_exn word.thread with
    | Ast1.Number n -> append_int n data
    | _ -> raise (Failure "constants can only be numbers")
  end else begin List.iter ~f:(fun word ->
    match word with
    | Ast1.Number n -> append_short n data
    | _ -> ()) word.thread
  end;
  word.length <- ((here data) - h)

let compile_postpone dict word data =
  match word with
  | Call s -> postpone_word dict s data
  | If | Else | Then | Begin | Until | While | Repeat | Again ->
      resolve_word dict (Ast1.of_word word) data
  | _ -> raise (Failure "postpone only valid for words")

let compile_bracket_tick dict word data =
  match word with
  | Call s -> compile_word_address dict s data
  | If | Else | Then | Begin | Until | While | Repeat | Again ->
      compile_word_address dict (Ast1.of_word word) data
  | _ -> raise (Failure "['] only valid for words")

let compile_char dict word data =
  match word with
  | Call s | Undefined s -> resolve_word dict "lit" data;
    append_int (Char.to_int (String.get s 0)) data
  | _ -> raise (Failure ("[char] only valid for words, got " ^ (of_word word)))

let rec process_thread dict words data =
  match words with
  | Postpone :: hd :: tl ->
      compile_postpone dict hd data; process_thread dict tl data
  | Bracket_tick :: hd :: tl ->
      compile_bracket_tick dict hd data; process_thread dict tl data
  | Char :: hd :: tl -> compile_char dict hd data; process_thread dict tl data
  | hd :: tl -> append_call dict hd data; process_thread dict tl data
  | [] -> ()

let handle_word (dict : Ast1.program) (word : Ast1.definition) data =
  add_header word data;
  let h = here data in
  if (word.deferred) then begin
    add_deferred data
  end else begin
    add_enter data;
    process_thread dict word.thread data;
    add_exit data
  end;
  word.length <- ((here data) - h)

let generate_word_code (dict : Ast1.program) (word : Ast1.definition) data =
  if (word.code)
  then handle_code_word word data
  else handle_word dict word data

let generate_program_code (p : Ast1.program) =
  let data = Buffer.create (64 * 1024) in
  Buffer.add_bytes data (Bytes.make 0x90 '\xff');
  let _result = List.fold
    ~init:(p, data)
    ~f:(fun acc word -> (generate_word_code (fst acc) word data); acc) p in
  (* let _: Ast1.program = if Logs.level () = Some Info then Ast1.print_program p else p in *)
  let cold = match Ast1.find_word p "cold" with
  | Some w -> w.address
  | None -> 0 in
  let reset_handler = match Ast1.find_word p "reset-handler" with
  | Some w -> w.address
  | None -> 0 in
  let header = Buffer.create 0x90 in
  append_int stack header;
  append_int (reset_handler + 1) header;
  Buffer.add_bytes header (Bytes.make (0x90 - 4 - 4 - (4 * 4)) '\xff');
  append_int (cold + 1) header;
  append_int (!latest + base) header;
  append_int ((here data) + base) header;
  append_int ram header;
  let final = Bytes.create (here data) in
  Buffer.blit ~src:header ~src_pos:0 ~dst:final ~dst_pos:0 ~len:0x90;
  Buffer.blit ~src:data ~src_pos:0x90 ~dst:final
    ~dst_pos:0x90 ~len:((here data) - 0x90);
  final
