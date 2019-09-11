(* tokens *)
%token EOF COLON SEMICOLON IMMEDIATE CODE END_CODE BACKSLASH CONSTANT
%token <string> WORD

(* start symbol *)
%start <Ast0.program> program

%%

definition:
  | value = WORD; CONSTANT; name = WORD { { name = name; words = [value] ; immediate = true ; code = true; constant = true } }
  | COLON; IMMEDIATE; words = list(WORD); SEMICOLON { { name = "immediate"; words = words ; immediate = false ; code = false; constant = false } }
  | COLON; COLON; words = list(WORD); SEMICOLON { { name = ":"; words = words ; immediate = false ; code = false; constant = false } }
  | COLON; CONSTANT; words = list(WORD); SEMICOLON { { name = "constant"; words = words ; immediate = false ; code = false; constant = false } }
  | COLON; SEMICOLON; words = list(WORD); SEMICOLON IMMEDIATE { { name = ";"; words = words ; immediate = true ; code = false; constant = false } }
  | COLON; name = WORD; words = list(WORD); SEMICOLON IMMEDIATE { { name = name; words = words ; immediate = true ; code = false; constant = false } }
  | COLON; name = WORD; words = list(WORD); SEMICOLON { { name = name; words = words ; immediate = false ; code = false; constant = false } }
  | CODE; name = WORD; words = list(WORD); END_CODE { { name = name; words = words ; immediate = false ; code = true; constant = false } }
  ;

program:
  | definitions = list(definition); EOF { definitions }
  ;

%%
