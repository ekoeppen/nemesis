open Core

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

let _ = begin

  (* enable pretty error messages *)
  Nemesis.Parser.pp_exceptions ();
  Nemesis.Parser.parse_file "test.ft"
  |> Nemesis.Ast1.of_ast0_program
  |> Nemesis.Gen.generate_program_code
  |> save
  (* |> hexdump *)
end
