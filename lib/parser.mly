%{
    open Ast
%}

%token <int> INT
%token <string> ID

%token EQUAL
%token IN
%token LET
%token IFZERO
%token FIX
%token LPAR
%token RPAR
%token SUM
%token SUB
%token MUL
%token DIV
%token FUN
%token ARROW
%token EOF
%token DEF
%token REC
%token DOTS
%token THEN
%token ELSE
%token LESS
%token GREATER
%token ASSIGN
%token DIFF
%token MATCH
%token WITH
%token BAR
%token AND
%token OR

%start <Ast.term> prog
%%

prog:
  | e = expr; EOF { e }
  ;

atomic:
  | i = INT { Constant i }
  | id = ID { Variable id }
  | LPAR; e = expr; RPAR { e }
  ;

_cond:
  | c1 = sum; LESS; c2 = _cond { Less(c1, c2) }
  | c1 = sum; GREATER; c2 = _cond { Greater(c1, c2) }
  | c1 = sum; EQUAL; c2 = _cond { Equal(c1, c2) }
  | c1 = sum; LESS; EQUAL; c2 = _cond { LessEqual(c1, c2) }
  | c1 = sum; GREATER; EQUAL; c2 = _cond { GreaterEqual(c1, c2) }
  | c1 = sum; DIFF; c2 = _cond { Different(c1, c2) }
  | s = sum { s }
  ;

cond:
  | c1 = _cond; AND; c2 = cond { And(c1, c2) }
  | c1 = _cond; OR; c2 = cond { Or(c1, c2) }
  | c = _cond { c }

sum:
  | e1 = sum; SUM; e2 = mul { Addition (e1, e2) }
  | e1 = sum; SUB; e2 = mul { Subtraction (e1, e2) }
  | m = mul { m }
  ;

mul:
  | e1 = mul; MUL; e2 = app { Multiplication (e1, e2) }
  | e1 = mul; DIV; e2 = app { Division (e1, e2) }
  | a = app { a }
  ;

app:
  | a1 = app; a2 = atomic { Application (a1, a2) }
  | a = atomic { a }
  ;

brch:
  | BAR; t1 = sum; ARROW; t2 = atomic; { (t1, t2) }
  ;

expr:
  | c = cond { c }
  | DEF; REC; id = ID; DOTS; vars = list(ID); ASSIGN; e1 = expr; IN e2 = expr { DefRec (id, vars, e1, e2) }
  | DEF; id = ID; DOTS; vars = list(ID); ASSIGN; e1 = expr; IN e2 = expr { Def (id, vars, e1, e2) }
  | FUN; idl = list(ID); ARROW; e = expr { Lambda (idl, e) }
  | LET; id = ID; ASSIGN; e1 = expr; IN e2 = expr { Let (id, e1, e2) }
  | IFZERO; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { IfZero (e1, e2, e3) }
  | MATCH; v = expr; WITH; l = list(brch) { Match (v, l) }
  | FIX; a = atomic { Fix (a) }
