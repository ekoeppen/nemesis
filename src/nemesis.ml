open Core
open Cmdliner

include Hexdump.Of_indexable (struct
  type t = Bytes.t
  let length = Bytes.length
  let get = Bytes.get
end)

module Gen_stm32_stc = Gen.Impl(Stm32_stc)
module Gen_msp430_dtc = Gen.Impl(Msp430_dtc)

let save output_file data =
  let f = Out_channel.create output_file in
  Out_channel.output f ~buf:data ~len:(Bytes.length data) ~pos:0;
  Out_channel.flush f;
  Out_channel.close f;
  data

let hexdump data =
  let s = Hexdump.to_string_hum data ~max_lines:100000 in
  Out_channel.output_string stdout s; Out_channel.output_char stdout '\n';
  data

let rec read_file f =
  let lines = In_channel.read_lines f in
  List.fold ~init:"" ~f:(fun s l ->
    if String.is_prefix ~prefix:"include" l then begin
      s ^ read_file (List.nth_exn (String.split ~on:' ' l) 1)
    end
    else s ^ l ^ "\n") lines

let read_files srcs =
  List.fold ~init:"" ~f:(fun s f -> s ^ read_file f) srcs

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let output_file =
  let doc = "Output file." in
  Arg.(value & opt string "forth.bin" & info ["o"] ~docv:"OUTPUT" ~doc)

let target =
  let doc = "Target. One of stm32-stc or msp430-dtc." in
  Arg.(value & opt string "stm32-stc" & info ["t"] ~docv:"TARGET" ~doc)

let srcs =
  let doc = "Source file(s)." in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"SOURCE" ~doc)

let logging =
  let env = Arg.env_var "NEMESIS_VERBOSITY" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())

let nemesis _logging target output_file srcs =
  let _: bytes =
    Forthparser.pp_exceptions ();
    read_files srcs
    |> Forthparser.parse_string
    |> Ast1.of_ast0_program
    |> (match target with
      | "msp430-dtc" -> Gen_msp430_dtc.generate_image
      | "stm32-stc" -> Gen_stm32_stc.generate_image
      | _ -> Gen_stm32_stc.generate_image)
    |> save output_file
  in Out_channel.flush stdout


let cmd =
  let doc = "Nemesis" in
  let exits = Term.default_exits in
  Term.(const nemesis $ logging $ target $ output_file $ srcs),
  Term.info "nemesis" ~doc ~exits

let () = Term.(eval cmd |> exit)
