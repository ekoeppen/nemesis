(* tokens *)
%token EOF
%token COLON
%token SEMICOLON
%token IMMEDIATE
%token CODE
%token END_CODE
%token CONSTANT
%token VARIABLE
%token BUFFER
%token SQUOTE_DEF
%token DOTQUOTE_DEF
%token <string> WORD
%token <string> STRING
%token <string> PRINT_STRING

(* start symbol *)
%start <Ast0.program> program

%%

definition:
  | value = WORD; CONSTANT; name = WORD
    { { name = name; words = [value] ; immediate = false ; kind = Constant } }
  | VARIABLE; name = WORD
    { { name = name; words = [] ; immediate = false ; kind = Variable } }
  | value = WORD; BUFFER; name = WORD
    { { name = name; words = [value] ; immediate = false ; kind = Buffer } }
  | SQUOTE_DEF; words = list(content); SEMICOLON IMMEDIATE
    { { name = "s\""; words = words ; immediate = true ; kind = Highlevel } }
  | DOTQUOTE_DEF; words = list(content); SEMICOLON IMMEDIATE
    { { name = ".\""; words = words ; immediate = true ; kind = Highlevel } }
  | COLON; IMMEDIATE; words = list(content); SEMICOLON
    { { name = "immediate"; words = words ; immediate = false ; kind = Highlevel } }
  | COLON; COLON; words = list(content); SEMICOLON
    { { name = ":"; words = words ; immediate = false ; kind = Highlevel } }
  | COLON; VARIABLE; words = list(content); SEMICOLON
    { { name = "variable"; words = words ; immediate = false ; kind = Highlevel } }
  | COLON; BUFFER; words = list(content); SEMICOLON
    { { name = "buffer:"; words = words ; immediate = false ; kind = Highlevel } }
  | COLON; CONSTANT; words = list(content); SEMICOLON
    { { name = "constant"; words = words ; immediate = false ; kind = Highlevel } }
  | COLON; SEMICOLON; words = list(content); SEMICOLON IMMEDIATE
    { { name = ";"; words = words ; immediate = true ; kind = Highlevel } }
  | COLON; name = WORD; words = list(content); SEMICOLON IMMEDIATE
    { { name = name; words = words ; immediate = true ; kind = Highlevel } }
  | COLON; name = WORD; words = list(content); SEMICOLON
    { { name = name; words = words ; immediate = false ; kind = Highlevel } }
  | CODE; name = WORD; words = list(content); END_CODE
    { { name = name; words = words ; immediate = false ; kind = Code } }
  ;

content:
  | v = WORD { v }
  | v = STRING { v }
  | v = PRINT_STRING { v }
  ;

program:
  | definitions = list(definition); EOF { definitions }
  ;

%%
