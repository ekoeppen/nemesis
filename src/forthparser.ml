include Nice_parser.Make(struct
  type result = Ast0.program
  type token = Menhir_parser.token
  exception ParseError = Menhir_parser.Error
  let parse = Menhir_parser.program
  include Forthlexer
end)
