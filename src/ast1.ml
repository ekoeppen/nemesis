open Base
open Core

type word =
  | Call of string
  | Number of int
  | String of string
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
  code : bool ;
  constant : bool;
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
  | String s -> "s\"" ^ s ^ "\""
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
    else begin if String.is_prefix s ~prefix:"s\""
      then String (s |> String.chop_prefix_exn ~prefix:"s\" " |> String.chop_suffix_exn ~suffix:"\"")
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
      code = d.code;
      constant = d.constant;
      thread = of_ast0_words p d.words;
      address = 0;
      length = 0;
    }) p

let print_thread t =
  List.iter ~f:(fun w -> Printf.printf "%s " (of_word w)) t

let print_definition (d : definition) =
  Printf.printf "%08x: (%04d) " d.address d.length;
  if (d.code) then Printf.printf "code " else Printf.printf ": ";
  Printf.printf "%s " d.name; print_thread d.thread;
  if (d.code) then Printf.printf "end-code " else Printf.printf "; ";
  if (d.immediate) then Printf.printf("immediate");
  Printf.printf "\n"

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
