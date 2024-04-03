open Ast

let rec indent n =
  String.make (2 * n) ' '

let rec string_of_op = function
  | Add -> "ADD"
  | Sub -> "SUB"
  | Equal -> "EQUAL"
  | Neq -> "NEQ"
  | Lt -> "LT"
  | Leq -> "LEQ"
  | Gt -> "GT"
  | Geq -> "GEQ"
  | And -> "AND"
  | Or -> "OR"
  | Not -> "NOT"

let rec string_of_typ = function
  | Num -> "num"
  | Bool -> "bool"
  | String -> "string"
  | Arr t -> "arr {" ^ string_of_typ t ^ "}"
  | Func (args, ret) -> "func { params: [" ^ String.concat ", " (List.map string_of_typ args) ^ "], rtype: " ^ string_of_typ ret ^ "}" 
  | None -> "none"

let rec string_of_expr = function
  | NumLit n -> "NUMLIT {" ^ string_of_int n ^ "}"
  | BoolLit b -> "BOOLLIT {" ^ string_of_bool b ^ "}"
  | StringLit s -> "STRINGLIT {" ^ s ^ "}" 
  | Id id -> "ID {" ^ id ^ "}"
  | Binop (e1, op, e2) -> "BINOP {" ^ string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2 ^ "}"
  | Unop (op, e) -> "UNOP {" ^ string_of_op op ^ string_of_expr e ^ "}"
  | Assign (id, e) -> "ASSIGN {" ^ id ^ " = " ^ string_of_expr e ^ "}"
  | Call (fn, args) -> "CALL {" ^ fn ^ ", [" ^ String.concat ", " (List.map string_of_expr args) ^ "]}"

let rec string_of_stmt = function
  | Expr expr -> "STMT {" ^ string_of_expr expr ^ "}"
  | Block stmts -> "BLOCK {" ^ String.concat ", " (List.map string_of_stmt stmts) ^ "}"
  | If (cond, s1, s2) -> "IF {" ^ string_of_expr cond ^ ", " ^ string_of_stmt s1 ^ ", " ^ string_of_stmt s2 ^ "}"
  | For (init, cond, update, s) -> "FOR {" ^ string_of_expr init ^ ", " ^ string_of_expr cond ^ ", " ^ string_of_expr update ^ ", " ^ string_of_stmt s ^ "}"
  | While (cond, s) -> "WHILE {" ^ string_of_expr cond ^ ", " ^ string_of_stmt s ^ "}"
  | Continue -> "CONTINUE"
  | Break -> "BREAK"
  | Return expr -> "RETURN {" ^ string_of_expr expr ^ "}"

let string_of_bind (typ, name) = "BIND {" ^ string_of_typ typ ^ ", " ^ name ^ "}"

let string_of_func_def (func_def : func_def ) = "FUNC_DEF {" ^ 
  "rtype: {" ^ string_of_typ func_def.rtyp ^ "}, " ^
  "fname: {" ^ func_def.fname ^ "}, " ^
  "formals: { [" ^ String.concat ", " (List.map string_of_bind func_def.formals) ^ "] }, " ^
  "body: { [" ^ String.concat ", " (List.map string_of_stmt func_def.body) ^ "] }"

let string_of_program (stmt_list, func_def_list) = "PROGRAM { " ^ String.concat ", " (List.map string_of_stmt stmt_list) ^ ", " ^ String.concat ", " (List.map string_of_func_def func_def_list)  ^ " }"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.expr Scanner.tokenize lexbuf in
  print_endline (string_of_program program)

(* let print_ast program =
  print_endline (string_of_program program)

let example_ast = (
  [(Num, "x"); (Bool, "y")],
  [
    { rtyp = Num;
      fname = "main";
      formals = [];
      locals = [];
      body = [
        Expr (NumLit 42);
        Expr (Binop (Id "x", Add, Id "y"));
        Expr (Call ("print", [Id "z"]))
      ]
    }
  ]
)

let _ = print_ast example_ast *)
