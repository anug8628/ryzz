open Ast

let rec indent n =
  String.make (2 * n) ' '

let rec string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Not -> "!"

let rec string_of_typ = function
  | Num -> "int"
  | Bool -> "bool"
  | String s -> "string " ^ s
  | Arr t -> "arr(" ^ string_of_typ t ^ ")"
  | Func (args, ret) -> "func(" ^ String.concat ", " (List.map string_of_typ args) ^ ") -> " ^ string_of_typ ret
  | None -> "none"

let rec string_of_expr = function
  | NumLit n -> string_of_int n
  | BoolLit b -> string_of_bool b
  | StringLit s -> "\"" ^ s ^ "\""
  | Id id -> id
  | Binop (e1, op, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2 ^ ")"
  | Unop (op, e) -> string_of_op op ^ string_of_expr e
  | Assign (id, e) -> id ^ " = " ^ string_of_expr e
  | Call (fn, args) -> fn ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"

let rec string_of_stmt ?(indent_level=1) = function
  | Expr expr -> indent indent_level ^ string_of_expr expr ^ ";"
  | Block stmts -> 
      let indented_stmts = List.map (fun stmt -> string_of_stmt ~indent_level:(indent_level+1) stmt) stmts in
      "{\n" ^ String.concat "\n" indented_stmts ^ "\n" ^ indent indent_level ^ "}"
  | If (cond, s1, s2) -> 
      indent indent_level ^ "if (" ^ string_of_expr cond ^ ")\n" ^ 
      string_of_stmt ~indent_level:(indent_level+1) s1 ^ "\n" ^ 
      indent indent_level ^ "else\n" ^ 
      string_of_stmt ~indent_level:(indent_level+1) s2
  | For (init, cond, update, s) -> 
      indent indent_level ^ "for (" ^ string_of_expr init ^ "; " ^ string_of_expr cond ^ "; " ^ string_of_expr update ^ ")\n" ^ 
      string_of_stmt ~indent_level:(indent_level+1) s
  | While (cond, s) -> 
      indent indent_level ^ "while (" ^ string_of_expr cond ^ ")\n" ^ 
      string_of_stmt ~indent_level:(indent_level+1) s
  | Continue -> indent indent_level ^ "continue;"
  | Break -> indent indent_level ^ "break;"
  | Return expr -> indent indent_level ^ "return " ^ string_of_expr expr ^ ";"

let string_of_bind (typ, name) = string_of_typ typ ^ " " ^ name

let string_of_func_def func_def =
  let formals_str = String.concat ", " (List.map string_of_bind func_def.formals) in
  let locals_str =
    if List.length func_def.locals > 0 then
      let indented_locals = List.map (fun bind -> indent 3 ^ string_of_bind bind) func_def.locals in
      "Locals:\n" ^ String.concat "\n" indented_locals ^ "\n"
    else
      "" in
  let body_str = String.concat "\n" (List.map (fun s -> indent 2 ^ s) (List.map (string_of_stmt ~indent_level:3) func_def.body)) in
  "func " ^ func_def.fname ^ "(" ^ formals_str ^ ") -> " ^ string_of_typ func_def.rtyp ^ "\n" ^ locals_str ^ body_str

let string_of_program (globals, funcs) =
  let globals_str = String.concat "\n" (List.map (fun s -> indent 2 ^ s) (List.map string_of_bind globals)) in
  let funcs_str = String.concat "\n\n" (List.map (fun s -> indent 2 ^ s) (List.map string_of_func_def funcs)) in
  "program\n" ^
  indent 1 ^ "Globals:\n" ^ globals_str ^ "\n\n" ^
  indent 1 ^ "Functions:\n" ^ funcs_str

let print_ast program =
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

let _ = print_ast example_ast
