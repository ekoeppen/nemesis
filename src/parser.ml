include Nice_parser.Make(struct
  type result = Ast0.program
  type token = Menhir_parser.token
  exception ParseError = Menhir_parser.Error
  let parse = Menhir_parser.program
  include Lexer
end)


(*===========================================================================*)
(* TESTS                                                                     *)
(* https://dune.readthedocs.io/en/stable/tests.html#inline-expectation-tests *)
(*===========================================================================*)

let%test_module _ = (module struct

  Printexc.record_backtrace false;;

end)
