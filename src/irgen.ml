

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let symbol_table = ref StringMap.empty

(* translate : Sast.program -> Llvm.module *)
let translate (stmts) =
  let map_to_str m = 
    let inners = List.map (fun (k, v) -> k ^ " -> " ^ (L.string_of_llvalue v)) (StringMap.bindings m)
    in "[" ^ (String.concat ", " inners) ^ "]" in

  let context = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Ryzz" in

  (* Get types from the context *)
  let float_t = L.float_type context
  and bool_t  = L.i1_type    context 
  and string_t = L.pointer_type (L.i8_type context) in

  let rec ltype_of_typ = function
      A.Num   -> float_t
    | A.Bool  -> bool_t
    | A.String -> string_t
    | A.None -> float_t
    | A.Func (params, rtype) -> 
      let formal_types = Array.of_list (List.map ltype_of_typ params) in 
      L.function_type (ltype_of_typ rtype) formal_types
  in

  let copy_map m = 
    StringMap.fold StringMap.add m StringMap.empty
  in

  let define_func fdecl = 
    let name = fdecl.sfname in
    let formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
    in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
    symbol_table := StringMap.add name (L.define_function name ftype the_module) !symbol_table in

  (* Fill in the body of the given function *)
  let rec build_function_body fdecl =
    let the_function = StringMap.find fdecl.sfname !symbol_table in
    let builder = L.builder_at_end context (L.entry_block the_function) in
  
  (* Allocate space for any locally declared variables and add the
  * resulting registers to our map *)
  let add_var (t, n) =
    let local_var = L.build_alloca (ltype_of_typ t) n builder in 
    symbol_table := StringMap.add n local_var !symbol_table;
  in

  let add_formal (t, n) p =
    L.set_value_name n p;
    let local = L.build_alloca (ltype_of_typ t) n builder in
    ignore (L.build_store p local builder);
    symbol_table := StringMap.add n local !symbol_table in

  let lookup n = try StringMap.find n !symbol_table 
  with Not_found -> raise (Failure (n ^ " Not Found"))
in

  let formals = List.iter2 add_formal fdecl.sformals
  (Array.to_list (L.params the_function)) in

  let rec build_expr builder ((t, e) : sexpr) = 
    print_endline(string_of_sexpr (t, e));
    print_endline(map_to_str !symbol_table);
    match e with
    SNumLit i  -> L.const_float float_t i
  | SBoolLit b  -> L.const_int bool_t (if b then 1 else 0)
  | SStringLit s -> L.const_string context s
  | SId s       -> L.build_load (lookup s) s builder
  | SUnop (op, e) ->
    let e' = build_expr builder e in
    (match op with
      A.Not -> L.build_neg  
    ) e' "tmp" builder
  | SBinop (e1, op, e2) ->
    let e1' = build_expr builder e1
    and e2' = build_expr builder e2 in
    (match op with
       A.Add     -> L.build_fadd
     | A.Sub     -> L.build_fsub
     | A.Times   -> L.build_fmul
     | A.Div     -> L.build_fdiv
     | A.Mod     -> L.build_frem
     | A.And     -> L.build_and
     | A.Or      -> L.build_or
     | A.Equal   -> L.build_icmp L.Icmp.Eq
     | A.Neq     -> L.build_icmp L.Icmp.Ne
     | A.Lt      -> L.build_icmp L.Icmp.Slt
     | A.Leq     -> L.build_icmp L.Icmp.Sle
     | A.Gt      -> L.build_icmp L.Icmp.Sgt
     | A.Geq     -> L.build_icmp L.Icmp.Sge
    ) e1' e2' "tmp" builder
  | SAssign (s, e) -> let e' = build_expr builder e in
    ignore(L.build_store e' (lookup s) builder); e'
  | SDecAssign ((t, s), e) ->
    let e' = build_expr builder e in
    let _ = add_var (t, s) in
    ignore(L.build_store e' (lookup s) builder); e'
  (* | SCall ("print", [e]) ->
    L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
      "printf" builder *)
  | SCall (f, args) ->
    let fdef = StringMap.find f !symbol_table in
    let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
    let result = f ^ "_result" in
    L.build_call fdef (Array.of_list llargs) result builder
in
let add_terminal builder instr =
  match L.block_terminator (L.insertion_block builder) with
    Some _ -> ()
  | None -> ignore (instr builder) in

  let rec build_stmt builder = function
        SBlock sl -> 
          let old_symbol_table = !symbol_table in
          symbol_table := copy_map !symbol_table;
          let res = List.fold_left build_stmt builder sl in
          symbol_table := old_symbol_table;
          res
      | SExpr e -> ignore(build_expr builder e); builder
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
      | SFuncDef fd -> 
        let _ = define_func fd in
        let old_symbol_table = !symbol_table in
        symbol_table := copy_map !symbol_table;
        let _ = build_function_body fd in
        symbol_table := old_symbol_table;
        builder
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb
      | SFor(init, cond, inc, body) -> 
        ignore (build_expr builder init);
        let for_bb = L.append_block context "for" the_function in
        let build_br_for = L.build_br for_bb in 
        ignore (build_br_for builder);
        let for_builder = L.builder_at_end context for_bb in
        let bool_val = build_expr for_builder cond in

        let body_bb = L.append_block context "for_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_for;
        ignore (build_expr (L.builder_at_end context body_bb) inc);

        let end_bb = L.append_block context "for_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb for_builder);

        L.builder_at_end context end_bb
      (* 
        for(init, cond, inc)
        
      init
      for:
        e code
        cond br 
      body: 
        body_stmt
        inc
        jmp for
      end:
      *)
      | _ -> builder

    in let func_builder = build_stmt builder (SBlock fdecl.sbody) in
    add_terminal func_builder (L.build_ret (L.const_int float_t 0)) in

  
  let main_func = {
    srtyp = A.Num;
    sfname = "main";
    sformals = [];
    sbody = List.rev stmts;
  } in
  let _ = define_func main_func in
  build_function_body main_func;
  the_module
    
 
      