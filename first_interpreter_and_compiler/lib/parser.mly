%{
  open Interpreter
%}

%token <int> INT
%token ADD
%token MULT

%token LPAR
%token RPAR

%token EOF

%left ADD
%left MUL

%start <Interpreter.expr> prog
%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Num i }
  | e1 = expr; ADD; e2 = expr { Add (e1, e2) }
  | e1 = expr; MULT; e2 = expr { Add (e1, e2) }
  | LPAR; e = expr; RPAR {e}
  ;
