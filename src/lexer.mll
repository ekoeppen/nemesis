{
open Menhir_parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}

(* regular expressions *)
let whitespace = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'
let ident = ['!'-'~']*

rule next_token = parse
  | eof { EOF }
  | whitespace+
    { next_token lexbuf }
  | newline
    { Lexing.new_line lexbuf; next_token lexbuf }
  | '\\'
    { line_comment lexbuf; next_token lexbuf }
  | "("
    { comment 0 lexbuf; next_token lexbuf }

  (* YOUR TOKENS HERE... *)
  | ':' { COLON }
  | ';' { SEMICOLON }
  | "constant" { CONSTANT }
  | "immediate" { IMMEDIATE }
  | "code" { CODE }
  | "end-code" { END_CODE }

  (* lex identifiers last, so keywords are not lexed as identifiers *)
  | ident as word { WORD word }

  (* no match? raise exception *)
  | _ as c { illegal c }

and comment nesting = parse
  | "("
    { comment (nesting+1) lexbuf }
  | ")"
    { if nesting > 0 then comment (nesting - 1) lexbuf }
  | eof
    { failwith "[lexer] unterminated comment at EOF" }
  | _
    { comment nesting lexbuf }


and line_comment = parse
  | newline
    { }
  | _
    { line_comment lexbuf}
