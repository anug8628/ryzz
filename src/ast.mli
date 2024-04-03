type op = Add | Sub | Equal | Neq | Lt | Leq | Gt | Geq | And | Or | Not | Mod

type typ = 
| Num 
| Bool 
| String
| Arr of typ 
| Func of typ list * typ
| None

type expr =
  | NumLit of float
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of op * expr
  | Assign of string * expr
  (* function call *)
  | Call of string * expr list

(* int x: name binding *)
type bind = typ * string

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt
  | FuncDef of {
      rtyp: typ;
      fname: string;
      formals: bind list;
      body: stmt list;
  }
  | Continue 
  | Break
  (* return *)
  | Return of expr


type program = stmt list

