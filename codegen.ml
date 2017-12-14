(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
module E = Exceptions

module StringMap = Map.Make(String)

let translate (elements, world) =
  let context = L.global_context () in
  let the_module = L.create_module context "Craft"
  and i32_t  = L.i32_type  context (*int*)
  and flt_t = L.float_type context
  and i8_t   = L.i8_type   context (*printf format string / 8 bit pointer*)
  and i1_t   = L.i1_type   context (*bool*)
  and str_t  = L.pointer_type (L.i8_type context) 
  and void_t = L.void_type context in
  let color_t = L.named_struct_type context "color_t" in
  L.struct_set_body color_t [|str_t|] false;
  let pair_t = L.named_struct_type context "pair_t" in
  L.struct_set_body pair_t [|i32_t; i32_t|] false;
  let elem_t = L.named_struct_type context "elem_t" in
  L.struct_set_body elem_t [|pair_t; pair_t; color_t; i32_t; flt_t|] false;
  let world_t = L.named_struct_type context "world_t" in
  L.struct_set_body world_t [|pair_t;color_t|] false;

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Float -> flt_t
    | A.Bool -> i1_t
    | A.Void -> void_t
    | A.Pair -> pair_t
    | A.Color -> color_t
  in

  (* let add_e_t = L.function_type (L.void_type context) [| (L.pointer_type elem_t) |] in
  let add_e = L.declare_function "add_element" add_e_t the_module in *)
  let world_func_t = L.function_type i32_t [||] in
  let world_func = L.declare_function "world" world_func_t the_module in

  let init_world_func_type = L.function_type (L.void_type context) [|L.pointer_type world_t|] in
  let init_world_func = L.declare_function "init_world" init_world_func_type the_module in

  let start_render_func_type = L.function_type (L.void_type context) [||] in
  let start_render_func = L.declare_function "startRender" start_render_func_type the_module in

  (* Helper functions *)
  let get_var_expr var_name var_list = 
    let func = fun (t,s,e) -> s = var_name in
      match List.filter func var_list with
        | (t,s,e) :: tl -> e
        | [] -> A.Noexpr
  in

  (* Declare each global variable; remember its value in a map *)
  (* let global_vars =
  let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals 
  in *)

 


  (* Define each function (arguments and return type) so we can call it *)
  (* let function_decls =
  let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions 
  in *)

  (* let element_decls = 
  let element_decl m eldecl =
      let name = eldecl.A.ename in
        let eltype = L.element_type (ltype_of_typ eldecl.A.typ) formal_types in
      StringMap.add name (L.define_element name eltype the_module, eldecl) m in
    List.fold_left element_decl StringMap.empty elements 
  in *)

  (* Fill in the body of the given function *)
  (* let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder 
  in *)
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
  (* let local_vars =
  let add_formal m (t, n) p = L.set_value_name n p;
    let local = L.build_alloca (ltype_of_typ t) n builder in
    ignore (L.build_store p local builder);
    StringMap.add n local m 
  in *)




    (* Return the value for a variable or formal argument *)
  (* let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
  in *)

    (* Construct code for an expression; return its value *)
  let rec expr builder = function
      A.ILiteral i -> L.const_int i32_t i
    (* | A.Fliteral f -> L.const_float flt_t f *)
    | A.SLiteral s -> L.const_string context s
    | A.BLiteral b -> L.const_int i1_t (if b then 1 else 0)
    | A.Noexpr -> L.const_int i32_t 0
    (* | A.Id s -> L.build_load (lookup s) s builder *)

    | A.Binop (e1, op, e2) ->
      let e1' = expr builder e1
      and e2' = expr builder e2 in
      if (L.type_of e1' = flt_t || L.type_of e2' = flt_t) 
      then
      (match op with
        A.Add     -> L.build_fadd
      | A.Sub     -> L.build_fsub
      | A.Mult    -> L.build_fmul
      | A.Div     -> L.build_fdiv
      | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
      | A.Neq     -> L.build_fcmp L.Fcmp.One
      | A.Less    -> L.build_fcmp L.Fcmp.Olt
      | A.Leq     -> L.build_fcmp L.Fcmp.Ole
      | A.Greater -> L.build_fcmp L.Fcmp.Ogt
      | A.Geq     -> L.build_fcmp L.Fcmp.Oge
      | _         -> raise (E.InvalidBinaryOperation) 
      ) e1' e2' "tmp" builder 
      else
      (match op with
        A.Add     -> L.build_add
      | A.Sub     -> L.build_sub
      | A.Mult    -> L.build_mul
      | A.Div     -> L.build_sdiv
      | A.And     -> L.build_and
      | A.Or      -> L.build_or
      | A.Equal   -> L.build_icmp L.Icmp.Eq
      | A.Neq     -> L.build_icmp L.Icmp.Ne
      | A.Less    -> L.build_icmp L.Icmp.Slt
      | A.Leq     -> L.build_icmp L.Icmp.Sle
      | A.Greater -> L.build_icmp L.Icmp.Sgt
      | A.Geq     -> L.build_icmp L.Icmp.Sge
      ) e1' e2' "tmp" builder

    | A.Unop (op, e) ->
      let e' = expr builder e in
      (match op with
        A.Neg     -> L.build_neg
      | A.Not     -> L.build_not
      ) e' "tmp" builder

    (* TODO: convert hex to rgb  *)
    | A.Cr (e) -> 
      let e' = expr builder e in
      let cr_ptr = L.build_alloca color_t "tmp" builder in
      let hex_ptr = L.build_struct_gep cr_ptr 0 "hex" builder in
      ignore (L.build_store e' hex_ptr builder);
      L.build_load cr_ptr "c" builder

    | A.Pr (e1, e2) -> 
      let e1' = expr builder e1 in
      let e2' = expr builder e2 in
      let pr_ptr = L.build_alloca pair_t "tmp" builder in
      let x_ptr = L.build_struct_gep pr_ptr 0 "x" builder in
      ignore (L.build_store e1' x_ptr builder);
      let y_ptr = L.build_struct_gep pr_ptr 1 "y" builder in
      ignore (L.build_store e2' y_ptr builder);
      L.build_load pr_ptr "p" builder

    (* | A.Assign (s, e) -> let e' = expr builder e in
      ignore (L.build_store e' (lookup s) builder); e' *)

    (* | A.Call (f, act) ->
      let (fdef, fdecl) = StringMap.find f function_decls in
      let actuals = List.rev (List.map (expr builder) (List.rev act)) in
      let result = (match fdecl.A.typ with A.Void -> "" *)
    (* | _ -> f ^ "_result") in
      L.build_call fdef (Array.of_list actuals) result builder *)
  in

  (* Invoke "f builder" if the current block doesn't already have a terminal (e.g., a branch). *)
  let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) 
  in
  
  (* Build the code for the given statement; return the builder for the statement's successor *)
  (* let rec stmt builder = function
        A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
        A.Void -> L.build_ret_void builder
      (* | A.New elem -> 
        let (t, name, t_check, pos) = elem in *)

       (* TODO: Comlete the NEW statement *)

      | _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
          let bool_val = expr builder predicate in
          let merge_bb = L.append_block context "merge" the_function in
          let then_bb = L.append_block context "then" the_function in
          add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
          (L.build_br merge_bb);
          let else_bb = L.append_block context "else" the_function in
          add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
          (L.build_br merge_bb);

          ignore (L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
        ignore (L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (stmt (L.builder_at_end context body_bb) body)
        (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr pred_builder predicate in

        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb
  in *)

  (* Build the code for each statement in the function *)
  (* let builder = stmt builder (A.Block fdecl.A.body) in *)
    (* Add a return if the last block falls off the end *)
    (* add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in *)

(*   let add_world_props m props builder =
    let add_prop m (t,n,e) = 
    let e' = expr builder e in 
    let prop = L.build_alloca (ltype_of_typ t) n builder in
    ignore (L.build_store e' prop builder); 
    StringMap.add n prop m in

    List.fold_left add_prop m props
  in

  let world_start world map = 
    let name = "world_start" in
    let func_type = L.function_type (L.pointer_type world_t) [||] in
    let func = L.define_function name func_type the_module in
    (StringMap.add name func map, func)
  in

  let world_start_func world map =
    let (map, func) = world_start world map in
    let builder = L.builder_at_end context (L.entry_block func) in
    let world_ptr = L.build_malloc world_t ("world_ptr") builder in 
    
    let world_size_ptr = L.build_struct_gep world_ptr 0 ("size_ptr") builder in
    let size_expr = get_var_expr "size" world.A.properties in 
    ignore (L.build_store (expr builder size_expr) world_size_ptr builder);
    (* TODO: convert hex to rgb *)
    let world_color_ptr = L.build_struct_gep world_ptr 1 ("color_ptr") builder in
    let color_expr = get_var_expr "color" world.A.properties in 
    ignore (L.build_store (expr builder color_expr) world_color_ptr builder);
    ignore (L.build_ret world_ptr builder);
    map
  in
   *)


  (* let main_map = world_start_func world StringMap.empty in  *)

  let main_func_type = L.function_type i32_t [||] in
  let main_func = L.define_function "main" main_func_type the_module in
  let main_func_builder = L.builder_at_end context (L.entry_block main_func) in
  (* let temp_ptr_world = StringMap.find "world_ptr" main_map in *)
  ignore (L.build_call world_func [||] "" main_func_builder);
  (* ignore (L.build_call init_world_func [|temp_ptr_world|] "" main_func_builder);
  ignore (L.build_call start_render_func [||] "" main_func_builder); *)
  ignore (L.build_ret (L.const_int i32_t 0) main_func_builder);


the_module