{
open Parser
open Exn
}

let white = [' ' '\t' '\r' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+

rule read =
    parse
    | white { read lexbuf }
    | int { INT (int_of_string (Lexing.lexeme lexbuf))}
    | "(" { LPAR }
    | ")" { RPAR }
    | "+" { SUM }
    | "-" { SUB }
    | "*" { MUL }
    | "/" { DIV }
    | "\\" { FUN }
    | "->" { ARROW }
    | "let" { LET }
    | "in" { IN }
    | ":" { DOTS }
    | ":=" { ASSIGN }
    | "=" { EQUAL }
    | "<>" { DIFF }
    | "&&" { AND }
    | "||" { OR }
    | "fix" { FIX }
    | "def" { DEF }
    | "rec" { REC }
    | "ifzero" { IFZERO }
    | "then" { THEN }
    | "else" { ELSE }
    | "match" { MATCH }
    | "with" { WITH }
    | "|" { BAR }
    | "<" { LESS }
    | ">" { GREATER }
    | ['a'-'z' 'A'-'Z' '_']+ { ID (Lexing.lexeme lexbuf) }
    | eof { EOF }
    | _ as lxm { raise @@ Exn(Lex, "unexpected char" ^ (String.make 1 lxm)) }
