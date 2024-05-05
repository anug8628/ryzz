type op = Add | Sub | Equal | Neq | Lt | Leq | Gt | Geq | And | Or | Not | Mod

type typ = 
| Num 
| Bool 
| String
| Arr of typ 
| Func of typ list * typ
| None

(* int x: name binding *)
type vdecl = typ * string

type expr =
  | NumLit of float
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of op * expr
  | DecAssign of vdecl * expr
  | Assign of string * expr
  (* function call *)
  | Call of string * expr list

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt
  | FuncDef of {
      rtyp: typ;
      fname: string;
      formals: vdecl list;
      body: stmt list;
  }
  | Continue 
  | Break
  (* return *)
  | Return of expr


type program = stmt list

let string_of_op = function
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
  | Mod -> "MOD"

let rec string_of_typ = function
  | Num -> "num"
  | Bool -> "bool"
  | String -> "string"
  | Arr t -> "arr {" ^ string_of_typ t ^ "}"
  | Func (args, ret) -> "func { params: [" ^ String.concat ", " (List.map string_of_typ args) ^ "], rtype: " ^ string_of_typ ret ^ "}" 
  | None -> "none"

let string_of_vdecl (typ, id) = "VDECL {" ^ string_of_typ typ ^ ", " ^ id ^ "}"

let rec string_of_expr = function
  | NumLit n -> "NUMLIT {" ^ string_of_float n ^ "}"
  | BoolLit b -> "BOOLLIT {" ^ string_of_bool b ^ "}"
  | StringLit s -> "STRINGLIT {" ^ s ^ "}" 
  | Id id -> "ID {" ^ id ^ "}"
  | Binop (e1, op, e2) -> "BINOP {" ^ string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2 ^ "}"
  | Unop (op, e) -> "UNOP {" ^ string_of_op op ^ string_of_expr e ^ "}"
  | Assign (id, e) -> "ASSIGN {" ^ id ^ " = " ^ string_of_expr e ^ "}"
  | DecAssign (vdecl, e) -> "DECASSIGN {" ^ string_of_vdecl vdecl ^ " = " ^ string_of_expr e ^ "}"
  | Call (fn, args) -> "CALL {" ^ fn ^ ", [" ^ String.concat ", " (List.map string_of_expr args) ^ "]}"

let string_of_bind (typ, name) = "BIND {" ^ string_of_typ typ ^ ", " ^ name ^ "}"
let rec string_of_stmt = function
  | Expr expr -> "STMT {" ^ string_of_expr expr ^ "}"
  | Block stmts -> "BLOCK {" ^ String.concat ", " (List.map string_of_stmt stmts) ^ "}"
  | If (cond, s1, s2) -> "IF {" ^ string_of_expr cond ^ ", " ^ string_of_stmt s1 ^ ", " ^ string_of_stmt s2 ^ "}"
  | For (init, cond, update, s) -> "FOR {" ^ string_of_expr init ^ ", " ^ string_of_expr cond ^ ", " ^ string_of_expr update ^ ", " ^ string_of_stmt s ^ "}"
  | While (cond, s) -> "WHILE {" ^ string_of_expr cond ^ ", " ^ string_of_stmt s ^ "}"
  | Continue -> "CONTINUE"
  | Break -> "BREAK"
  | Return expr -> "RETURN {" ^ string_of_expr expr ^ "}"
  | FuncDef func_def -> "FUNC_DEF {" ^ 
  "rtype: {" ^ string_of_typ func_def.rtyp ^ "}, " ^
  "fname: {" ^ func_def.fname ^ "}, " ^
  "formals: { [" ^ String.concat ", " (List.map string_of_bind func_def.formals) ^ "] }, " ^
  "body: { [" ^ String.concat ", " (List.map string_of_stmt func_def.body) ^ "] }"

let string_of_program stmt_list = "PROGRAM { [" ^ String.concat ", " (List.map string_of_stmt stmt_list) ^ "] }"