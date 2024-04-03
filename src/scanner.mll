{ 
  open Parser 
  open Lexing

  let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1
    }
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ascii = [' '-'!' '#'-'[' ']'-'~']
let exponent = ('E' | 'e') digit+

let number = digit+ ('.' digit+)? exponent?
let id = letter (letter | digit | '_')*
let string = '"' ascii* '"'

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
  (* COMMENTS *)
| "//" { single_line_comment lexbuf }
| "/*" { multi_line_comment lexbuf }
  (* Symbols *)
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LSQUARE }
| ']' { RSQUARE }
| '{' { LCURLY }
| '}' { RCURLY }
| ',' { COMMA }
| ';' { SEMI }
  (* Operators *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIV }
| '%' { MOD }
| '=' { ASSIGN }
| "==" { EQ }
| "!=" { NEQ }
| "<" { LT }
| "<=" { LEQ }
| ">" { GT }
| ">=" { GEQ }
| "and" { AND }
| "or" { OR }
| "not" { NOT }
  (* Control Flow *)
| "if" { IF }
| "else" { ELSE }
| "for" { FOR }
| "while" { WHILE }
| "continue" { CONTINUE }
| "break" { BREAK }
  (* Types *)
| "num" { NUM }
| "bool" { BOOL }
| "string" { STRING }
| "func" { FUNC }
| "none" { NONE }
  (* Functions *)
| "->" { ARROW }
| "return" { RETURN }
  (* LITERALS *)
| number as num {NUMLIT (float_of_string num)}
| "true" {BOOLLIT true}
| "false" {BOOLLIT false}
| string as str {STRINGLIT (String.sub str 1 (String.length str - 2))}
| id as var {ID var}
| eof { EOF }

and single_line_comment = parse
  | '\n' { next_line lexbuf; tokenize lexbuf }
  | eof { EOF }
  | _ { single_line_comment lexbuf }


and multi_line_comment = parse
  | "/*" { tokenize lexbuf }
  | '\n' { next_line lexbuf; multi_line_comment lexbuf }
  | _ { multi_line_comment lexbuf }