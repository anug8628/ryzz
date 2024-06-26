open Ast

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.program Scanner.tokenize lexbuf in
    print_endline (string_of_program program)