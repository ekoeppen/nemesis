open Ast1
open Core

module Impl (T : Target.Intf) (C : Target.Conf) = struct

  let latest = ref 0
  let latest_ram = ref C.ram
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
        T.append_call w.address data
    | None -> raise (Failure ("Word not found " ^ word))

  let compile_word_address dict word data =
    match Ast1.find_word dict word with
    | Some w ->
        if (w.address = 0)
        then raise (Failure ("Word " ^ w.name ^ " not resolved yet"));
        resolve_word dict "lit" data;
        T.append_address w.address data
    | None -> raise (Failure ("Word not found " ^ word))

  let postpone_word dict word data =
    match Ast1.find_word dict word with
    | Some w ->
        if (w.address = 0)
        then raise (Failure ("Word " ^ w.name ^ " not resolved yet"));
        if (w.immediate) then begin
          T.append_call w.address data
        end else begin
          resolve_word dict "lit" data;
          T.append_address w.address data;
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
        T.align data
    | Printstring s ->
        resolve_word dict "(s\")" data;
        Buffer.add_char data (char_of_int (String.length s));
        Buffer.add_string data s;
        resolve_word dict "type" data;
        T.align data
    | Number n -> resolve_word dict "lit" data; T.append_number n data
    | Immediate -> Data.update_char (!latest + 4) '\xfe' data
    | If ->
        resolve_word dict "?branch" data;
        push_here (Data.here data);
        T.append_placeholder_address data
    | Else ->
        resolve_word dict "branch" data;
        T.append_placeholder_address data;
        T.update_address (pop_here ()) ((Data.here data) + C.base) data;
        push_here ((Data.here data) - T.cell_size)
    | Then -> T.update_address (pop_here ()) ((Data.here data) + C.base) data
    | Begin -> push_here (Data.here data)
    | Again ->
        resolve_word dict "branch" data;
        T.append_address (pop_here () + C.base) data
    | Until ->
        resolve_word dict "?branch" data;
        T.append_address (pop_here () + C.base) data
    | While ->
        resolve_word dict "?branch" data;
        push_here (Data.here data);
        T.append_placeholder_address data
    | Repeat ->
        resolve_word dict "branch" data;
        T.update_address (pop_here ())
        ((Data.here data) + C.base + T.cell_size) data;
        T.append_address (pop_here () + C.base) data
    | Undefined s -> raise (Failure ("Undefined word " ^ s))
    | _ -> ()

  let handle_code_word dict (word : Ast1.definition) data =
    let h = Data.here data in
    List.iter ~f:(fun word -> match word with
      | Ast1.Number n -> T.append_code n data
      | Ast1.Call w -> resolve_word dict w data
      | _ -> ()) word.thread;
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
      T.append_number (Char.to_int (String.get s 0)) data
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
    let h = Data.here data in
    let docol = Ast1.find_word_or_zero dict "docol" in
    let exit = Ast1.find_word_or_zero dict "exit" in
    T.add_enter docol data;
    process_thread dict word.thread data;
    T.add_exit exit data;
    word.length <- ((Data.here data) - h)

  let handle_constant (word : Ast1.definition) data =
    match List.hd_exn word.thread with
    | Ast1.Number n -> T.append_inline_constant n data
    | _ -> raise (Failure "constants can only be numbers")

  let handle_variable data =
    T.append_inline_constant !latest_ram data;
    latest_ram := !latest_ram + T.cell_size

  let handle_buffer (word : Ast1.definition) data =
    match List.hd_exn word.thread with
    | Ast1.Number w ->
        T.append_inline_constant !latest_ram data;
        latest_ram := !latest_ram + w
    | _ -> raise (Failure "buffer size needs to be a number")

  let generate_word_code (dict : Ast1.program) (word : Ast1.definition) data =
    latest := T.add_header word data C.base !latest;
    match word.kind with
    | Code -> handle_code_word dict word data
    | Highlevel -> handle_word dict word data
    | Variable -> handle_variable data
    | Constant -> handle_constant word data
    | Buffer -> handle_buffer word data

  let generate_program_code (p : Ast1.program) data =
    let _result = List.fold ~init:(p, data)
      ~f:(fun acc word -> (generate_word_code (fst acc) word data); acc) p in
    match Logs.level () with
    | Some Logs.Info -> let _p: definition list  = Ast1.print_program p in ()
    | _ -> ()

  let generate_image (p : Ast1.program) =
    let data = T.create_image_data in
    generate_program_code p data;
    T.finalize_image_data p data !latest !latest_ram C.base C.info C.user

end
