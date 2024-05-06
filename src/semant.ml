(* Semantic checking for the Ryzz compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

let symbol_table = ref StringMap.empty

let check (stmts) = 
  let copy_map m = 
    StringMap.fold StringMap.add m StringMap.empty
  in
  let rec check_expr e = 
    match e with
    | NumLit l -> (Num, SNumLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | StringLit l -> (String, SStringLit l)
    | Id var -> (type_of_identifier var, SId var)
    | Assign(var, e) as ex ->
      let lt = type_of_identifier var
      and (rt, e') = check_expr e in
      let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr ex
      in
      (check_assign lt rt err, SAssign(var, (rt, e')))
    | Binop(e1, op, e2) as e ->
      let (t1, e1') = check_expr e1
      and (t2, e2') = check_expr e2 in
      let err = "illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e
      in
      (* All binary operators require operands of the same type*)
      if t1 = t2 then
        (* Determine expression type based on operator and operand types *)
        let t = match op with
            Add | Sub | Times | Div | Mod when t1 = Num -> Num
          | Equal | Neq -> Bool
          | Lt | Leq | Gt | Geq when t1 = Num -> Bool
          | And | Or when t1 = Bool -> Bool
          | _ -> raise (Failure err)
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
      else raise (Failure err)
    | Unop(op, e) ->
      let (t, e') = check_expr e in
      let err = "illegal binary operator " ^ string_of_typ t ^ " " ^ string_of_op op  ^ " in " ^ string_of_expr e in
      let t1 = match op with 
        Not when t = Bool -> Bool
      | _ -> raise (Failure err)
      in
      (t, SUnop(op, (t1, e')))
    | Call(fname, args) as call ->
      let fd = find_func fname in
      let param_length = List.length fd.formals in
      if List.length args != param_length then
        raise (Failure ("expecting " ^ string_of_int param_length ^
                        " arguments in " ^ string_of_expr call))
      else let check_call (ft, _) e =
             let (et, e') = check_expr e in
             let err = "illegal argument found " ^ string_of_typ et ^
                       " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
             in (check_assign ft et err, e')
        in
        let args' = List.map2 check_call fd.formals args
        in (fd.rtyp, SCall(fname, args'))
    | DecAssign((t, id), e) ->
      let (t', e') = check_expr e in
      let err = "illegal assignment " ^ string_of_typ t ^ " = " ^
                string_of_typ t' ^ " in " ^ string_of_expr e
      in let res = check_assign(check_assign lt rt err, SDecAssign((t, id), (t', e'))) in
      symbol_table := StringMap.add id t !symbol_table;
      res
  in

  let check_bool_expr e =
    let (t, e') = check_expr e in
    match t with
    | Bool -> (t, e')
    |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
  in
  let rec check_stmt_list stmts = 
    let old_symbol_table = !symbol_table in
    symbol_table := copy_map !symbol_table;
    let res = 
      match stmts with
      | [] -> []
      | s :: sl -> check_stmt s :: check_stmt_list sl
    in
    symbol_table := old_symbol_table;
    res
  and check_stmt stmt = 
    match stmt with
    | Block sl -> SBlock (check_stmt_list sl)
    | Expr e -> SExpr (check_expr e)
    | If(e, st1, st2) -> SIf(check_bool_expr e, check_stmt st1, check_stmt st2)
    | While(e, st) -> SWhile(check_bool_expr e, check_stmt st)
    | Return e ->
      let (t, e') = check_expr e in
      if t = func.rtyp then SReturn (t, e')
      else raise (
          Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                   string_of_typ func.rtyp ^ " in " ^ string_of_expr e))