open Base
open Core

type definition = {
  name : string ;
  words : string list ;
  immediate : bool;
  code : bool;
  constant : bool;
}

type program = definition list

let find program w =
  List.exists ~f:(fun d -> String.equal (String.lowercase d.name) (String.lowercase w)) program

let print_words w =
  List.iter ~f:(fun w -> Out_channel.output_string stdout w; Out_channel.output_char stdout ' ') w;
  Out_channel.output_char stdout '\n'

let print_definition (d : definition) =
  Out_channel.output_string stdout d.name; Out_channel.output_string stdout ": ";
  print_words d.words

let print_program (p : program) =
  List.iter ~f:(fun d -> print_definition d) p;
  Out_channel.output_string stdout "ok.\n";
  p
