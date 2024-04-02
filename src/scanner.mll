{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ascii = [' '-'!' '#'-'[' ']'-'~']
let exponent = ('E' | 'e') digit+

let number = digit+ ('.' digit+)? exponent?
let id = letter (letter | digit | '_')*
let string = '"' ascii* '"'

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

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
| '/' { DIVIDE }
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
| "true" {BOOLIT true}
| "false" {BOOLIT false}
| string as str {STRINGLIT (String.sub str 1 (String.length str - 2))}
| id as var {ID var}
| eof { EOF }

and single_line_comment = parse
  | '\n' { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ { read_single_line_comment lexbuf }


and multi_line_comment = parse
  | "/*" { read_token lexbuf }
  | '\n' { next_line lexbuf; read_multi_line_comment lexbuf }
  | eof { raise (SyntaxError ("Unexpected EOF - terminate your comment.")) }
  | _ { read_multi_line_comment lexbuf }