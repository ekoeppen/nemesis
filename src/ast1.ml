open Base
open Core

type word =
  | Call of string
  | Number of int
  | String of string
  | Printstring of string
  | Immediate
  | Postpone
  | Bracket_tick
  | Char
  | If
  | Else
  | Then
  | Begin
  | Until
  | While
  | Repeat
  | Again
  | Undefined of string

type definition = {
  name : string ;
  immediate : bool ;
  thread : word list ;
  kind : Ast0.deftype;
  mutable address : int ;
  mutable length : int ;
}

type program = definition list

let to_number s =
  let s = Str.replace_first (Str.regexp "^[#]") "" s
    |> Str.replace_first (Str.regexp "^[%]") "0b"
    |> Str.replace_first (Str.regexp "^[&]") "0o"
    |> Str.replace_first (Str.regexp "^[$]") "0x" in
  try Some (int_of_string s)
  with _ -> None

let of_word w =
  match w with
  | Call s -> "[" ^ s ^ "]"
  | Number n -> Printf.sprintf "%04x" n
  | Immediate -> "IMMEDIATE"
  | Postpone -> "POSTPONE"
  | Bracket_tick -> "[']"
  | String s -> "s\" " ^ s ^ "\""
  | Printstring s -> ".\" " ^ s ^ "\""
  | Char -> "[CHAR]"
  | If -> "IF"
  | Else -> "ELSE"
  | Then -> "THEN"
  | Begin -> "BEGIN"
  | Until -> "UNTIL"
  | While -> "WHILE"
  | Repeat -> "REPEAT"
  | Again -> "AGAIN"
  | Undefined s -> s ^ "?"

let of_string d s =
  match String.lowercase s with
  | "postpone" -> Postpone
  | "[']" -> Bracket_tick
  | "immediate" -> Immediate
  | "[char]" -> Char
  | "if" -> If
  | "else" -> Else
  | "then" -> Then
  | "begin" -> Begin
  | "until" -> Until
  | "while" -> While
  | "repeat" -> Repeat
  | "again" -> Again
  | w -> if Ast0.find d w
    then Call w
    else begin
      if String.is_prefix s ~prefix:"s\""
      then String (s |> String.chop_prefix_exn ~prefix:"s\" " |> String.chop_suffix_exn ~suffix:"\"")
      else if String.is_prefix s ~prefix:".\""
      then Printstring (s |> String.chop_prefix_exn ~prefix:".\" " |> String.chop_suffix_exn ~suffix:"\"")
      else match to_number s with
      | Some n -> Number n
      | None -> Undefined w
    end

let of_ast0_words (p : Ast0.program) (w : string list) =
  List.map ~f:(fun d -> of_string p d) w

let of_ast0_program (p : Ast0.program) =
  List.map ~f:(fun d -> {
      name = d.name;
      immediate = d.immediate;
      thread = of_ast0_words p d.words;
      address = 0;
      length = 0;
      kind = d.kind;
    }) p

let print_thread t =
  List.iter ~f:(fun w -> Printf.printf "%s " (of_word w)) t

let print_definition (d : definition) =
  Printf.printf "%08x: (%04d) " d.address d.length;
  match d.kind with
  | Ast0.Code ->
      Printf.printf "code %s " d.name; print_thread d.thread;
      Printf.printf "end-code\n"
  | Ast0.Highlevel ->
      Printf.printf ": %s " d.name; print_thread d.thread;
      Printf.printf ";\n"
  | Ast0.Variable -> Printf.printf "variable %s\n" d.name
  | Ast0.Buffer ->
      Printf.printf "%s buffer: %s\n" (of_word (List.hd_exn d.thread)) d.name
  | Ast0.Constant ->
      Printf.printf "%s constant %s\n" (of_word (List.hd_exn d.thread)) d.name

let print_program (p : program) =
  List.iter ~f:(fun d -> print_definition d) p;
  Out_channel.output_string stdout "ok.\n";
  p

let rec find_word d w =
  match d with
  | hd :: tl ->
      if (String.equal (String.lowercase hd.name) (String.lowercase w))
      then Some hd
      else find_word tl w
  | [] -> None

let find_exn d w =
  match find_word d w with
  | Some word -> word
  | None -> raise (Failure ("Word " ^ w ^ " not found"))

let find_word_or_zero d w =
  match find_word d w with
  | Some w -> w.address
  | None -> 0
