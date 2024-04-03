open Ast

let rec string_of_op op =
  match op with
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


let rec string_of_typ typ =
  match typ with
  | Num -> "num"
  | Bool -> "bool"
  | String s -> "string(" ^ s ^ ")"
  | Arr t -> string_of_typ t ^ " [] " ^ "arr"
  | Func (args, ret) ->
      "func" ^ String.concat "(" (List.map string_of_typ args) ^ ") -> " ^ string_of_typ ret 
  | None -> "none"

  let rec string_of_expr expr =
    match expr with
    | NumLit n -> string_of_int n
    | BoolLit b -> string_of_bool b
    | Id id -> id
    | Binop (e1, op, e2) ->
        "(" ^ string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2 ^ ")"
    | Unop (op, e) ->
        string_of_op op ^ string_of_expr e
    | Assign (id, e) ->
        id ^ " = " ^ string_of_expr e
    | Call (fname, args) ->
        fname ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"

(*
todo: add string of stmt    
*)

let string_of_bind (typ, id) =
  string_of_typ typ ^ " " ^ id

(*ToDO: write string of func*)

(*ToDO: write string of program*)
