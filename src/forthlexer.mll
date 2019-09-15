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
  | whitespace+ { next_token lexbuf }
  | newline { Lexing.new_line lexbuf; next_token lexbuf }
  | '\\' { line_comment lexbuf; next_token lexbuf }
  | '(' { comment lexbuf; next_token lexbuf }
  | "s\" " [ ^ '"' ]+ "\"" { STRING (Lexing.lexeme lexbuf) }

  (* YOUR TOKENS HERE... *)
  | ':' { COLON }
  | ';' { SEMICOLON }
  | "constant" { CONSTANT }
  | "defer" { DEFER }
  | "immediate" { IMMEDIATE }
  | "code" { CODE }
  | "end-code" { END_CODE }
  | ": s\"" { SQUOTE_DEF }

  (* lex identifiers last, so keywords are not lexed as identifiers *)
  | ident as word { WORD word }

  (* no match? raise exception *)
  | _ as c { illegal c }

and comment = parse
  | ')' { }
  | eof { failwith "[lexer] unterminated comment at EOF" }
  | _ { comment lexbuf }

and line_comment = parse
  | newline { }
  | _ { line_comment lexbuf}
