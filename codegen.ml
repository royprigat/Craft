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





let translate (globals, funcs, events, elements, world) =
  let context = L.global_context () in
  let the_module = L.create_module context "Craft"
  and i32_t  = L.i32_type  context (*int*)
  and flt_t = L.float_type context
  (* and i8_t   = L.i8_type   context (*printf format string / 8 bit pointer*) *)
  and void_t = L.void_type context
  and i1_t   = L.i1_type   context (*bool*) 
  and str_t  = L.pointer_type (L.i8_type context) in 
  let color_t = L.named_struct_type context "color_t" in
  L.struct_set_body color_t [|i32_t; i32_t; i32_t|] false;
  let pair_t = L.named_struct_type context "pair_t" in
  L.struct_set_body pair_t [|i32_t; i32_t|] false;
  let elem_t = L.named_struct_type context "elem_t" in
  L.struct_set_body elem_t [|str_t; pair_t; pair_t; str_t; i32_t; i32_t|] false;
  let world_t = L.named_struct_type context "world_t" in
  L.struct_set_body world_t [|pair_t;str_t|] false;

  (* Global map of elements  *)
  (* let fill_elem_map m element = 
    StringMap.add (element.A.ename ^ "_element") element m in
  let elements_helper_map = List.fold_left fill_elem_map StringMap.empty elements in *)

  (* Global map of events *)
  let fill_event_map m event =
    StringMap.add (event.A.evname ^ "_event") event m in
  let events_helper_map = List.fold_left fill_event_map StringMap.empty events in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Float -> flt_t
    | A.Bool -> i1_t
    | A.Pair -> pair_t
    | A.Color -> color_t
    | A.String -> str_t
    | A.Void -> void_t
  in


  (* Using it for testing *)
  (* let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in *)

  let add_e_t = L.function_type (L.void_type context) [| (L.pointer_type elem_t) |] in
  let add_e = L.declare_function "add_element" add_e_t the_module in

  let world_func_t = L.function_type i32_t [||] in
  let world_func = L.declare_function "world" world_func_t the_module in

  let init_world_func_type = L.function_type (L.void_type context) [|L.pointer_type world_t|] in
  let init_world_func = L.declare_function "init_world" init_world_func_type the_module in

 (*  let start_render_func_type = L.function_type (L.void_type context) [||] in
  let start_render_func = L.declare_function "startRender" start_render_func_type the_module in *)

  let add_event_func_type = L.function_type (L.void_type context) [| L.pointer_type (L.function_type (L.void_type context) [||]) |] in
  let add_event_func = L.declare_function "addEventfn" add_event_func_type the_module in

  let c_print_func_type = L.function_type (L.void_type context) [||] in
  let c_print_func = L.declare_function "test_print" c_print_func_type the_module in

  let is_key_pressed_func_type = L.function_type i32_t [|str_t|] in (* correct return type? Bool??? *)
  let is_key_pressed_func = L.declare_function "isPressed" is_key_pressed_func_type the_module in

 (*  (* let delete_elem_func_type = L.function_type (L.void_type context) [|str_t|] in *)
  let delete_elem_func_type = L.function_type (L.pointer_type elem_t) [|str_t|] in
  let delete_elem_func = L.declare_function "delete_element" delete_elem_func_type the_module in *)

  let move_func_type = L.function_type (L.void_type context) [|str_t; str_t|] in
  let move_func = L.declare_function "move" move_func_type the_module in

  (* Helper functions *)
  let get_var_expr var_name var_list = 
    let func = fun (_,s,_) -> s = var_name in
      match List.filter func var_list with
        | (_,_,e) :: tl -> e     (*removed:  :: tl  *)
        | [] -> A.Noexpr
        (* | _ -> A.Noexpr *)
  in

  let rec string_of_expr = function
    | A.SLiteral(s) -> s
    | A.Cr(c) -> string_of_expr c
    | A.Keypress(s) -> string_of_expr s
    | A.Id (s) -> s
    | _ -> ""
  in

  let get_var_decl_value = function 
      A.ILiteral i -> L.const_int i32_t i
    | A.BLiteral b -> L.const_int i1_t (if b then 1 else 0)
    | A.Noexpr    -> L.const_int i32_t 0
    | _ -> L.const_int i32_t 0  
  in

  



 
    (* Construct code for an expression; return its value *)
  let rec expr builder map = function
      A.ILiteral i -> L.const_int i32_t i
    | A.FLiteral f -> L.const_float flt_t f 
    | A.SLiteral s -> L.const_string context s
    | A.BLiteral b -> L.const_int i1_t (if b then 1 else 0)
    | A.Noexpr -> L.const_int i32_t 0
    | A.Keypress s -> (expr builder map s)
    | A.Id s -> L.build_load (StringMap.find s map) s builder

    | A.Binop (e1, op, e2) ->
      print_string ("test_binop");
      let e1' = expr builder map e1
      and e2' = expr builder map e2 in
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
      let e' = expr builder map e in
      (match op with
        A.Neg     -> L.build_neg
      | A.Not     -> L.build_not
      ) e' "tmp" builder

    | A.Cr (e) -> 
      let e' = expr builder map e in
      let cr_ptr = L.build_alloca color_t "tmp" builder in
      let hex_ptr = L.build_struct_gep cr_ptr 0 "hex" builder in
      ignore (L.build_store e' hex_ptr builder);
      L.build_load cr_ptr "c" builder

    | A.Pr (e1, e2) -> 
      let e1' = expr builder map e1 in
      let e2' = expr builder map e2 in
      let pr_ptr = L.build_alloca pair_t "tmp" builder in
      let x_ptr = L.build_struct_gep pr_ptr 0 "x" builder in
      ignore (L.build_store e1' x_ptr builder);
      let y_ptr = L.build_struct_gep pr_ptr 1 "y" builder in
      ignore (L.build_store e2' y_ptr builder);
      L.build_load pr_ptr "p" builder

    | A.Assign (e1, e2) ->
      print_string ("test_assign");
      let e' = expr builder map e2 in (* should bribg back calculated value *)
      (* let e' = L.const_int i32_t 25 in *)
      print_string ("test10");

      (match e1 with
        |A.Id (s) -> L.build_store e' (StringMap.find s map) builder
        |A.PAccess (s1,s2,s3) ->
          (* let x_or_y = string_of_expr e in *)
          (* let x_or_y = "y" in *)
          (match s2 with
            | "pos" ->
              (match s3 with 
                | "x" ->  print_string ("test5");
                          let elem_ptr = StringMap.find (s1 ^ "_element") map in
                            let pos_ptr = L.build_struct_gep elem_ptr 2 ("elem5_pos_ptr") builder in 
                              let x_ptr = L.build_struct_gep pos_ptr 0 ("x_ptr") builder in
                                L.build_store e' x_ptr builder;
                | "y" ->  print_string ("test6");
                          let elem_ptr = StringMap.find (s1 ^ "_element") map in
                            let pos_ptr = L.build_struct_gep elem_ptr 2 ("elem6_pos_ptr") builder in 
                              let y_ptr = L.build_struct_gep pos_ptr 1 ("y_ptr") builder in
                                L.build_store e' y_ptr builder;
                | _ -> L.const_int i32_t 0
              )
            | "size" -> 
              (match s3 with
                  | "x" ->  print_string ("test7");
                            let elem_ptr = StringMap.find (s1 ^ "_element") map in
                              let size_ptr = L.build_struct_gep elem_ptr 2 ("elem7_size_ptr") builder in 
                                let x_ptr = L.build_struct_gep size_ptr 0 ("x_ptr") builder in
                                  L.build_store e' x_ptr builder;
                  | "y" -> print_string ("test8");
                          let elem_ptr = StringMap.find (s1 ^ "_element") map in
                            let size_ptr = L.build_struct_gep elem_ptr 2 ("elem8_size_ptr") builder in 
                              let y_ptr = L.build_struct_gep size_ptr 1 ("y_ptr") builder in
                                L.build_store e' y_ptr builder;
                   | _ -> L.const_int i32_t 0
              )
            | _ ->  L.const_int i32_t 0    
          )

        |  _ -> L.const_int i32_t 0  
      ) 
    | A.PAccess (s1,s2,s3) ->
      (* let x_or_y = string_of_expr e in (*either x or y*)  *)
      (* let x_or_y = "y" in *)
      (match s3 with
        | "x" ->
          (match s2 with 
            | "pos" -> print_string ("test1"); 
                      let elem_ptr = StringMap.find (s1 ^ "_element") map in
                        let pos_ptr = L.build_struct_gep elem_ptr 2 ("elem1_pos_ptr") builder in 
                          let x_ptr = L.build_struct_gep pos_ptr 0 ("x_ptr") builder in
                              L.build_load x_ptr "x" builder (*return the x value in llvm type*)

            | "size" -> print_string ("test2"); 
                        print_string (s1^"_element");
                        print_string ("tessssttttt");
                        let elem_ptr = StringMap.find (s1 ^ "_element") map in
                          let size_ptr = L.build_struct_gep elem_ptr 2 ("elem2_size_ptr") builder in 
                            let x_ptr = L.build_struct_gep size_ptr 0 ("x_ptr") builder in
                              L.build_load x_ptr "x" builder 
            | _ -> L.const_int i32_t 0
          ) 
        | "y" ->
          (match s2 with 
            | "pos" -> print_string ("test3"); 
                      let elem_ptr = StringMap.find (s1 ^ "_element") map in
                        let pos_ptr = L.build_struct_gep elem_ptr 2 ("elem3_pos_ptr") builder in 
                          let y_ptr = L.build_struct_gep pos_ptr 1 ("y_ptr") builder in
                            L.build_load y_ptr "y" builder (*return the y value in llvm type*)
            | "size" -> print_string ("test4"); 
                        let elem_ptr = StringMap.find (s1 ^ "_element") map in
                          let size_ptr = L.build_struct_gep elem_ptr 2 ("elem4_size_ptr") builder in 
                            let y_ptr = L.build_struct_gep size_ptr 1 ("y_ptr") builder in
                              L.build_load y_ptr "y" builder 
            | _ -> L.const_int i32_t 0            
          )
        | _ -> L.const_int i32_t 0  
      )
    | A.CAccess (_,_) -> L.const_int i32_t 0 (*to stop warning*)
    | A.Call (f, act) ->
      print_string("expr_Call\n");
      let func = StringMap.find f map in
      let actuals = List.rev (List.map (expr builder map) (List.rev act)) in
      L.build_call func (Array.of_list actuals) f builder

  in
 
    (* Invoke "f builder" if the current block doesn't already have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) 
    in

 
  
  (* Build the code for the given statement; return the builder for the statement's successor *)
  let rec stmt builder main_func map = function
        A.Block sl -> List.fold_left (fun builder s -> stmt builder main_func map s) builder sl
      | A.Expr e -> print_string("test12"); ignore (expr builder map e); builder
      | A.Return e -> ignore(L.build_ret (expr builder map e) builder); builder
      | A.New elem -> 
        let (e_typ, _, e_pos) = elem in

        let elem_name = (e_typ ^ "_element") in
        let build_elem_func_name = (elem_name ^ "_build") in
        let build_elem_func = StringMap.find build_elem_func_name map in
        let elem_ptr = L.build_call build_elem_func [||] "test_run" builder in
        
        (* add the new Element position *)
        let pos_ptr = L.build_struct_gep elem_ptr 2 (elem_name ^ "_pos_ptr") builder in
        ignore (L.build_store (expr builder map e_pos) pos_ptr builder); 

        (* Call add_element function *)
        ignore (L.build_call add_e [|elem_ptr|] "" builder);
        builder

      | A.ECall ("add_event", event_name) -> 
        print_string ("start_e_call\n");
        
        let event = StringMap.find (event_name ^ "_event") events_helper_map in
        
        let condition = string_of_expr event.A.condition in (*condition is now "UP"*)

        let first_elem = List.hd event.A.eformals in (*it's a string. ex player which is now the name*)
        (* let elem_ptr = StringMap.find (first_elem ^ "_element") map in *)

        (* let elem_name = (first_elem ^ "_element") in *)
       
        (* let first_stmt = List.hd event.A.action in *)
        (* let event_action_spec = stmt builder map first_stmt in *)

        (* assume we have keypress-up for now *)
        (* let event_func_type = L.function_type (L.void_type context) [||] in *)
        let event_func_type = L.function_type (L.void_type context) [||] in
        let event_func = L.define_function (condition ^ "_event_func") event_func_type the_module in
        let event_builder = L.builder_at_end context (L.entry_block event_func) in (*event_func runs a new basic block. trigggered by fucntion pointer in C*)
        (* let condition = string_of_expr event.A.condition in (*condition is now "UP"*) *)
        (* let str_ptr = L.build_alloca str_t "test_str_ptr" new_builder in 
         *)
       
        (*  let str_ptr = L.build_global_stringptr "UP" ("test_str_ptr") new_builder in *)
        let cond_str_ptr = L.build_global_stringptr condition ("condition_str_ptr") event_builder in
        let elem_name_str_ptr = L.build_global_stringptr first_elem (first_elem ^ "_str_ptr") event_builder in
  
        (*call the c function and get a yes or no*)
        let return_val = L.build_call is_key_pressed_func [|cond_str_ptr|] "" event_builder in (*stored in new basic block*)
   
        let is_pressed_bb = L.append_block context ("pressed_" ^ condition) event_func in
        let merge_bb = L.append_block context ("not_pressed_" ^ condition) event_func in

        let is_pressed_builder = L.builder_at_end context is_pressed_bb in
        (* let elem_ptr = L.build_call delete_elem_func [|elem_name_str_ptr|] "" is_pressed_builder in *)
        
        (* let elem_ptr = L.build_call delete_elem_func [|elem_name_str_ptr|] "" is_pressed_builder in *)
        ignore(L.build_call move_func [|elem_name_str_ptr;cond_str_ptr|] "" is_pressed_builder);

        (* ignore(stmt is_pressed_builder map first_stmt); *)
        (* issues pos is not added here, it's done in NEW. *)
        (* biggest problem is i do not access an the latest updated values just rerendeing initial pos and trying to update that.. *)
        (* ignore (L.build_call add_e [|elem_ptr|] "" is_pressed_builder); *)
      
        ignore (L.build_br merge_bb is_pressed_builder);

        let compare_instruction = L.build_icmp L.Icmp.Eq return_val (L.const_int i32_t 1) ("key_pressed_"^condition) event_builder in
        ignore(L.build_cond_br compare_instruction is_pressed_bb merge_bb event_builder);

        ignore (L.build_call c_print_func [||] "" event_builder); (*stored in new basic block*)
        ignore (L.build_ret_void event_builder); (*need it...not sure why*)
      
        let some_builder = L.builder_at_end context merge_bb in
        ignore (L.build_ret_void some_builder); (*???*)

        ignore (L.build_call add_event_func [|event_func|] "" builder); (*giving C the pointer and it will call the func pointer*)
        print_string ("end_e_call\n");

        (* ignore (L.build_ret_void is_pressed_builder);  *)        
        (*  L.builder_at_end context merge_bb *) (*which builder*)
        (* some_builder (*not using this new is seen... *)*)
        builder

    | A.ECall (_,_) -> builder
    | A.Return _ -> builder
    | A.Condition (_,_) -> builder
    | A.If (predicate, then_stmt, else_stmt) ->
        let bool_val = expr builder map predicate in
        let merge_bb = L.append_block context "merge" main_func in

        let then_bb = L.append_block context "then" main_func in
        add_terminal (stmt (L.builder_at_end context then_bb) main_func map then_stmt)
        (L.build_br merge_bb);

        let else_bb = L.append_block context "else" main_func in
        add_terminal (stmt (L.builder_at_end context else_bb) main_func map else_stmt)
        (L.build_br merge_bb);

        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb
      
    | A.While (predicate, body) ->
        print_string("stmt_while");
        let pred_bb = L.append_block context "while" main_func in
        ignore (L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" main_func in
        add_terminal (stmt (L.builder_at_end context body_bb) main_func map body)
        (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr pred_builder map predicate in

        let merge_bb = L.append_block context "merge" main_func in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb

    | A.For (e1, e2, e3, body) -> stmt builder main_func map
      ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )

    
  in


  (* Declare each global variable; remember its value in a map *)
  let global_vars_map =
    let global_var m (_, n, e) =
      let e' = get_var_decl_value e in
      StringMap.add n (L.define_global n e' the_module) m in
    List.fold_left global_var StringMap.empty globals 
  in



  let function_decls_map = 
    let function_decl map fdecl =
      let name = fdecl.A.fname
      and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      let func = L.define_function name ftype the_module in
      StringMap.add name func map 
    in
    List.fold_left function_decl global_vars_map funcs
  in

let functions_map = 
  let build_function_body m fdecl = 
    let the_function = StringMap.find fdecl.A.fname m in
    let func_builder = L.builder_at_end context (L.entry_block the_function) in

    let map = 
        let add_formal map (t,n) p = L.set_value_name n p;
          let local = L.build_alloca (ltype_of_typ t) n func_builder in
          ignore (L.build_store p local func_builder);
          StringMap.add n local map 
        in

        let add_local map (t,n,e) =
          let e' = get_var_decl_value e in
          let local_var = L.build_alloca (ltype_of_typ t) n func_builder in
          ignore(L.build_store e' local_var func_builder);
          StringMap.add n local_var map 
        in

        let formals = List.fold_left2 add_formal m fdecl.A.formals
          (Array.to_list (L.params the_function)) in
        List.fold_left add_local formals fdecl.A.locals
      in
    map(* build_function_body returns the filled map *)
  in
  List.fold_left build_function_body function_decls_map funcs
in



  (* let elements_map =  *)
  let build_element_body (m, builder) element =
    (* let the_element = StringMap.find (element.A.ename ^ "_element") elements_helper_map in *)

    (* ex: player_element *)
    let elem_name = (element.A.ename ^ "_element") in

    (* function used by new keyword to create element and add to world *)
    let build_elem_func_name = (elem_name ^ "_build") in
    let build_elem_func_type = L.function_type (L.pointer_type elem_t) [||] in
    let build_elem_func = L.define_function build_elem_func_name build_elem_func_type the_module in
    let new_builder = L.builder_at_end context (L.entry_block build_elem_func) in

    (* Element struct pointer *)
    (* let elem_ptr = L.build_malloc elem_t (elem_name ^ "_ptr") elem_builder in *)
    let elem_ptr = L.build_malloc elem_t (elem_name ^ "_ptr") new_builder in

    (* Element name *)
    let elem_name_str_ptr = L.build_global_stringptr element.A.ename (elem_name ^ "_id_str_ptr") new_builder in
    let elem_name_ptr = L.build_struct_gep elem_ptr 0 (elem_name ^ "_name_ptr") new_builder in
    ignore (L.build_store elem_name_str_ptr elem_name_ptr new_builder);

    (* Element size *)
    let elem_size_ptr = L.build_struct_gep elem_ptr 1 (elem_name ^ "_size_ptr") new_builder in
    let size_expr = get_var_expr "size" element.A.e_properties in 
    ignore (L.build_store (expr new_builder m size_expr) elem_size_ptr new_builder);

    (* Element color *)
    let color_expr = get_var_expr "color" element.A.e_properties in
    let color_str = string_of_expr color_expr in
    let elem_color_str_ptr = L.build_global_stringptr color_str (elem_name ^ "_color_str_ptr") new_builder in
    let color_ptr = L.build_struct_gep elem_ptr 3 (elem_name ^ "_color_ptr") new_builder in
    ignore (L.build_store elem_color_str_ptr color_ptr new_builder);

    (* Element direction *)
    let elem_direction_ptr = L.build_struct_gep elem_ptr 4 (elem_name ^ "_direction_ptr") new_builder in
    let direction_expr = get_var_expr "direction" element.A.e_properties in 
    ignore (L.build_store (expr new_builder m direction_expr) elem_direction_ptr new_builder);

    (* Element speed *)
    let elem_speed_ptr = L.build_struct_gep elem_ptr 5 (elem_name ^ "_speed_ptr") new_builder in
    let speed_expr = get_var_expr "speed" element.A.e_properties in 
    ignore (L.build_store (expr new_builder m speed_expr) elem_speed_ptr new_builder);


    ignore (L.build_ret elem_ptr new_builder); (*the functions return value*)

    let m = StringMap.add build_elem_func_name build_elem_func m in (*add the function so can call it in "NEW"*)


    (StringMap.add (elem_name) elem_ptr m, builder)
  in


 
  (* CREATE WORLD *)
  let world_start_func world map main_func builder =
  
    let world_ptr = L.build_malloc world_t ("world_ptr") builder in

    
    (* World size struct and pointer *)
    let world_size_ptr = L.build_struct_gep world_ptr 0 ("size_ptr") builder in
    let size_expr = get_var_expr "size" world.A.w_properties in 
    ignore (L.build_store (expr builder map size_expr) world_size_ptr builder);

    (* World color struct and pointer *)
    let color_expr = get_var_expr "color" world.A.w_properties in
    let color_str = string_of_expr color_expr in
    let world_color_str_ptr = L.build_global_stringptr color_str "color_str_ptr" builder in
    let color_ptr = L.build_struct_gep world_ptr 1 "color_ptr" builder in
    ignore (L.build_store world_color_str_ptr color_ptr builder);

    (* World locals*)
    let map = 
      let add_local map (t,s,e) =
        print_string("test_add_local");
        let e' = expr builder map e in
        let local_var = L.build_alloca (ltype_of_typ t) s builder in
            ignore (L.build_store e' local_var builder);
            StringMap.add s local_var map 
      in
      List.fold_left add_local map world.A.init_locals
    in



    (* World statements *)
    let world_stmt_list = world.A.init_body in
    (* ignore(List.fold_left (fun b s -> stmt b main_func map s) builder world_stmt_list); *)
    let builder = List.fold_left (fun b s -> stmt b main_func map s) builder world_stmt_list in

    (* ignore (L.build_ret builder); *)
    (world_ptr, map, builder)
  in



  (* MAIN FUNCTION *)
  let main_func_type = L.function_type i32_t [||] in
  let main_func = L.define_function "main" main_func_type the_module in
  let main_func_builder = L.builder_at_end context (L.entry_block main_func) in

  let (main_map, main_func_builder) = List.fold_left (fun (m,b) e -> build_element_body (m, b) e) (functions_map, main_func_builder) elements in

  let (world_ptr, main_map, main_func_builder) = world_start_func world main_map main_func main_func_builder in

  ignore (L.build_call init_world_func [|world_ptr|] "" main_func_builder);
  ignore (L.build_call world_func [||] "" main_func_builder);
  ignore (L.build_ret (L.const_int i32_t 0) main_func_builder);
 

the_module