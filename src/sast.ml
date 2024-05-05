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
      rtyp: typ;
      fname: string;
      formals: vdecl list;
      body: sstmt list;
  }
  | SContinue 
  | SBreak
  (* return *)
  | SReturn of sexpr


type program = sstmt list

