(* tokens *)
%token EOF
%token COLON
%token SEMICOLON
%token IMMEDIATE
%token CODE
%token END_CODE
%token CONSTANT
%token SQUOTE_DEF
%token <string> WORD
%token <string> STRING

(* start symbol *)
%start <Ast0.program> program

%%

definition:
  | value = WORD; CONSTANT; name = WORD
    { { name = name; words = [value] ; immediate = true ; deferred = false ; code = true; constant = true } }
  | SQUOTE_DEF; words = list(content); SEMICOLON IMMEDIATE
    { { name = "s\""; words = words ; immediate = true ; deferred = false ; code = false; constant = false } }
  | COLON; IMMEDIATE; words = list(content); SEMICOLON
    { { name = "immediate"; words = words ; immediate = false ; deferred = false ; code = false; constant = false } }
  | COLON; COLON; words = list(content); SEMICOLON
    { { name = ":"; words = words ; immediate = false ; deferred = false ; code = false; constant = false } }
  | COLON; CONSTANT; words = list(content); SEMICOLON
    { { name = "constant"; words = words ; immediate = false ; deferred = false ; code = false; constant = false } }
  | COLON; SEMICOLON; words = list(content); SEMICOLON IMMEDIATE
    { { name = ";"; words = words ; immediate = true ; deferred = false ; code = false; constant = false } }
  | COLON; name = WORD; words = list(content); SEMICOLON IMMEDIATE
    { { name = name; words = words ; immediate = true ; deferred = false; code = false; constant = false } }
  | COLON; name = WORD; words = list(content); SEMICOLON
    { { name = name; words = words ; immediate = false ; deferred = false ; code = false; constant = false } }
  | CODE; name = WORD; words = list(content); END_CODE
    { { name = name; words = words ; immediate = false ; deferred = false ; code = true; constant = false } }
  ;

content:
  | v = WORD { v }
  | v = STRING { v }
  ;

program:
  | definitions = list(definition); EOF { definitions }
  ;

%%
