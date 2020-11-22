open Ast1
open Core

module Target = Stm32_stc

let latest = ref 0
let base = 0x08000000
let ram = 0x20000080
let stack = 0x20004000

let here_stack = Stack.create ()

let push_here n =
  Stack.push here_stack n

let pop_here () =
  Stack.pop_exn here_stack

let resolve_word dict word data =
  match Ast1.find_word dict word with
  | Some w ->
      if (w.address = 0)
      then raise (Failure ("Word " ^ w.name ^ " not resolved yet"));
      Target.append_call w.address data
  | None -> raise (Failure ("Word not found " ^ word))

let compile_word_address dict word data =
  match Ast1.find_word dict word with
  | Some w ->
      if (w.address = 0)
      then raise (Failure ("Word " ^ w.name ^ " not resolved yet"));
      resolve_word dict "lit" data;
      Target.append_address w.address data
  | None -> raise (Failure ("Word not found " ^ word))

let postpone_word dict word data =
  match Ast1.find_word dict word with
  | Some w ->
      if (w.address = 0)
      then raise (Failure ("Word " ^ w.name ^ " not resolved yet"));
      if (w.immediate) then begin
        Target.append_call w.address data
      end else begin
        resolve_word dict "lit" data;
        Target.append_address w.address data;
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
      Target.align data
  | Number n -> resolve_word dict "lit" data; Target.append_number n data
  | Immediate -> Data.update_char (!latest + 4) '\xfe' data
  | If ->
      resolve_word dict "?branch" data;
      push_here (Data.here data);
      Target.append_placeholder_address data
  | Else ->
      resolve_word dict "branch" data;
      Target.append_placeholder_address data;
      Target.update_address (pop_here ()) ((Data.here data) + base) data;
      push_here ((Data.here data) - Target.cell_size)
  | Then -> Target.update_address (pop_here ()) ((Data.here data) + base) data
  | Begin -> push_here (Data.here data)
  | Again ->
      resolve_word dict "branch" data;
      Target.append_address (pop_here () + base) data
  | Until ->
      resolve_word dict "?branch" data;
      Target.append_address (pop_here () + base) data
  | While ->
      resolve_word dict "?branch" data;
      push_here (Data.here data);
      Target.append_placeholder_address data
  | Repeat ->
      resolve_word dict "branch" data;
      Target.update_address (pop_here ())
        ((Data.here data) + base + Target.cell_size) data;
      Target.append_address (pop_here () + base) data
  | _ -> ()

let add_header (word : Ast1.definition) data =
  Target.align data;
  let l = Data.here data in
  Target.append_address (!latest + base) data;
  latest := l;
  Buffer.add_char data (if (word.immediate) then '\xfe' else '\xff');
  Buffer.add_char data '\xff';
  Buffer.add_char data (char_of_int (String.length word.name));
  Buffer.add_string data word.name;
  Target.align data;
  word.address <- (Data.here data) + base

let handle_code_word (word : Ast1.definition) data =
  add_header word data;
  let h = Data.here data in
  if (word.constant) then begin
    Target.append_inline_constant data;
    match List.hd_exn word.thread with
    | Ast1.Number n -> Target.append_number n data
    | _ -> raise (Failure "constants can only be numbers")
  end else begin List.iter ~f:(fun word ->
    match word with
    | Ast1.Number n -> Target.append_code n data
    | _ -> ()) word.thread
  end;
  word.length <- ((Data.here data) - h)

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
    Data.append_int (Char.to_int (String.get s 0)) data
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
  let h = Data.here data in
  Target.add_enter data;
  process_thread dict word.thread data;
  Target.add_exit data;
  word.length <- ((Data.here data) - h)

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
  Data.append_int stack header;
  Data.append_int (reset_handler + 1) header;
  Buffer.add_bytes header (Bytes.make (0x90 - 4 - 4 - (4 * 4)) '\xff');
  Data.append_int (cold + 1) header;
  Data.append_int (!latest + base) header;
  Data.append_int ((Data.here data) + base) header;
  Data.append_int ram header;
  let final = Bytes.create (Data.here data) in
  Buffer.blit ~src:header ~src_pos:0 ~dst:final ~dst_pos:0 ~len:0x90;
  Buffer.blit ~src:data ~src_pos:0x90 ~dst:final
    ~dst_pos:0x90 ~len:((Data.here data) - 0x90);
  final
