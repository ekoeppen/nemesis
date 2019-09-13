open Core
open Cmdliner

include Hexdump.Of_indexable (struct
  type t = Bytes.t
  let length = Bytes.length
  let get = Bytes.get
end)

let save data =
  let f = Out_channel.create "meta.bin" in
  Out_channel.output f ~buf:data ~len:(Bytes.length data) ~pos:0;
  Out_channel.flush f;
  Out_channel.close f;
  data

let hexdump data =
  let s = Hexdump.to_string_hum data in
  Out_channel.output_string stdout s; Out_channel.output_char stdout '\n';
  data

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let srcs =
  let doc = "Source file(s)." in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"SOURCE" ~doc)

let logging =
  let env = Arg.env_var "NEMESIS_VERBOSITY" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())

let nemesis _logging srcs =
  let _ =
  Forthparser.pp_exceptions ();
  let s = List.fold ~init:"" ~f:(fun s f -> s ^ (In_channel.read_all f)) srcs in
  Forthparser.parse_string s
  |> Ast1.of_ast0_program
  |> Gen.generate_program_code
  |> save
  (* |> hexdump *)
  in ()


let cmd =
  let doc = "Nemesis" in
  let exits = Term.default_exits in
  Term.(const nemesis $ logging $ srcs),
  Term.info "nemesis" ~doc ~exits

let () = Term.(eval cmd |> exit)
