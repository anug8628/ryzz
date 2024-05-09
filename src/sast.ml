open Ast

type sexpr = typ * expr_detail
and expr_detail =
  | SNumLit of float
  | SBoolLit of bool
  | SStringLit of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of op * sexpr
  | SDecAssign of vdecl * sexpr
  | SAssign of string * sexpr
  (* function call *)
  | SCall of string * sexpr list

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SFuncDef of {
      srtyp: typ;
      sfname: string;
      sformals: vdecl list;
      sbody: sstmt list;
  }
  | SContinue 
  | SBreak
  (* return *)
  | SReturn of sexpr

type program = sstmt list

(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SNumLit(l) -> string_of_float l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SStringLit(s) -> s
      | SId(s) -> s
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SDecAssign((t, id), e) -> string_of_typ t ^ " " ^ id ^ string_of_sexpr e
      | SUnop(op, e) -> 
        string_of_op op ^ " " ^ string_of_sexpr e
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    ) ^ ")"

let rec string_of_sstmt sstmt = 
  match sstmt with
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SFor(e1, e2, e3, s) -> "for (" ^ string_of_sexpr e1 ^ ", " ^ string_of_sexpr e2 ^ ", " ^ string_of_sexpr e3 ^ ") " ^ string_of_sstmt s
  | SContinue -> "continue"
  | SBreak -> "break"
  | SFuncDef fd -> string_of_typ fd.srtyp ^ " " ^
      fd.sfname ^ "(" ^ String.concat ", " (List.map snd fd.sformals) ^
      ")\n{\n" ^
      String.concat "" (List.map string_of_sstmt fd.sbody) ^
      "}\n"

let string_of_sprogram sstmt_list =
  "\n\nSementically checked program: \n\n" ^
  String.concat "\n" (List.map string_of_sstmt sstmt_list)