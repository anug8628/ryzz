type op = Add | Sub | Equal | Neq | Lt | Leq | Gt | Geq | And | Or | Not 

type typ = 
| Num 
| Bool 
| String of string 
| Arr of typ 
| Func of typ list * typ
| None

type expr =
  | NumLit of int
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of op * expr
  | Assign of string * expr
  (* function call *)
  | Call of string * expr list

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt
  | Continue 
  | Break
  (* return *)
  | Return of expr

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * func_def list