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


let print_map m =
  print_string ("Map:\n");
  let print_key k v =
    print_string (k ^ "\n")
  in
  StringMap.iter print_key m;;



let translate (globals, funcs, events, elements, world) =
  let context = L.global_context () in
  let the_module = L.create_module context "Craft"
  and i32_t  = L.i32_type  context (*int*)
  and flt_t = L.float_type context
  and i8_t   = L.i8_type   context (*printf format string / 8 bit pointer*)
  and i1_t   = L.i1_type   context (*bool*)
  and str_t  = L.pointer_type (L.i8_type context) 
  and void_t = L.void_type context in
  let color_t = L.named_struct_type context "color_t" in
  L.struct_set_body color_t [|i32_t; i32_t; i32_t|] false;
  let pair_t = L.named_struct_type context "pair_t" in
  L.struct_set_body pair_t [|i32_t; i32_t|] false;
  let elem_t = L.named_struct_type context "elem_t" in
  L.struct_set_body elem_t [|str_t; pair_t; pair_t; str_t|] false;
  let world_t = L.named_struct_type context "world_t" in
  L.struct_set_body world_t [|pair_t;str_t|] false;

  (* Global map of elements  *)
  let fill_elem_map m element = 
    StringMap.add (element.A.ename ^ "_element") element m in
  let elements_helper_map = List.fold_left fill_elem_map StringMap.empty elements in

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
  in


  (* Using it for testing *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let add_e_t = L.function_type (L.void_type context) [| (L.pointer_type elem_t) |] in
  let add_e = L.declare_function "add_element" add_e_t the_module in

  let world_func_t = L.function_type i32_t [||] in
  let world_func = L.declare_function "world" world_func_t the_module in

  let init_world_func_type = L.function_type (L.void_type context) [|L.pointer_type world_t|] in
  let init_world_func = L.declare_function "init_world" init_world_func_type the_module in

  let start_render_func_type = L.function_type (L.void_type context) [||] in
  let start_render_func = L.declare_function "startRender" start_render_func_type the_module in

  let c_test_func_type = L.function_type (L.void_type context) [| L.pointer_type (L.function_type (L.void_type context) [||]) |] in
  let c_test_func = L.declare_function "testfn" c_test_func_type the_module in

  let c_print_func_type = L.function_type (L.void_type context) [||] in
  let c_print_func = L.declare_function "test_print" c_print_func_type the_module in

  let is_key_pressed_func_type = L.function_type i32_t [|str_t|] in (* correct return type? Bool??? *)
  let is_key_pressed_func = L.declare_function "isPressed" is_key_pressed_func_type the_module in

  let delete_elem_func_type = L.function_type (L.void_type context) [|str_t|] in
  let delete_elem_func = L.declare_function "delete_element" delete_elem_func_type the_module in

  (* Helper functions *)
  let get_var_expr var_name var_list = 
    let func = fun (t,s,e) -> s = var_name in
      match List.filter func var_list with
        | (t,s,e) :: tl -> e
        | [] -> A.Noexpr
  in

  let rec string_of_expr = function
    | A.SLiteral(s) -> s
    | A.Cr(c) -> string_of_expr c
    | A.Keypress(s) -> string_of_expr s
    | A.Id (s) -> s
  in

  let rec get_var_decl_value = function 
      A.ILiteral i -> L.const_int i32_t i
    | A.BLiteral b -> L.const_int i1_t (if b then 1 else 0)
    | A.Noexpr    -> L.const_int i32_t 0
    | _ -> L.const_int i32_t 0 
  in

  



 
    (* Construct code for an expression; return its value *)
  let rec expr builder map = function
      A.ILiteral i -> L.const_int i32_t i
    (* | A.Fliteral f -> L.const_float flt_t f *)
    | A.SLiteral s -> L.const_string context s
    | A.BLiteral b -> L.const_int i1_t (if b then 1 else 0)
    | A.Noexpr -> L.const_int i32_t 0
    | A.Keypress s -> (expr builder map s)
    | A.Id s -> L.build_load (StringMap.find s map) s builder

    | A.Binop (e1, op, e2) ->
      let e1' = expr builder map e1
      and e2' = expr builder map e2 in
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

    | A.PAccess (s1,s2,e) ->
      let x_or_y = string_of_expr e in (*either x or y*)
      (match x_or_y with
        | "x" ->
          (match s2 with 
            | "pos" -> let elem_ptr = StringMap.find (s1 ^ "_element") map in
                        let pos_ptr = L.build_struct_gep elem_ptr 2 ("elem_pos_ptr") builder in 
                          let x_ptr = L.build_struct_gep pos_ptr 0 ("x_ptr") builder in
                              L.build_load x_ptr "x" builder; (*return the x value in llvm type*)

            | "size" -> let elem_ptr = StringMap.find (s1 ^ "_element") map in
                          let size_ptr = L.build_struct_gep elem_ptr 2 ("elem_size_ptr") builder in 
                            let x_ptr = L.build_struct_gep size_ptr 0 ("x_ptr") builder in
                              L.build_load x_ptr "x" builder; (*NEED SEMI COLON????*)
          ) 
        | "y" ->
          (match s2 with 
            | "pos" -> let elem_ptr = StringMap.find (s1 ^ "_element") map in
                        let pos_ptr = L.build_struct_gep elem_ptr 2 ("elem_pos_ptr") builder in 
                          let y_ptr = L.build_struct_gep pos_ptr 1 ("y_ptr") builder in
                            L.build_load y_ptr "y" builder; (*return the y value in llvm type*)
            | "size" -> let elem_ptr = StringMap.find (s1 ^ "_element") map in
                          let size_ptr = L.build_struct_gep elem_ptr 2 ("elem_size_ptr") builder in 
                            let y_ptr = L.build_struct_gep size_ptr 1 ("y_ptr") builder in
                              L.build_load y_ptr "y" builder;             
          ) 
      )


    | A.Assign (e1, e2) ->
      let e' = expr builder map e2 in (* should bribg back calculated value *)
      (* let e' = L.const_int i32_t 25 in *)

      (match e1 with
        |A.PAccess (s1,s2,e) ->
          let x_or_y = string_of_expr e in 
          (match s2 with
            | "pos" ->
              (match x_or_y with 
                | "x" ->  let elem_ptr = StringMap.find (s1 ^ "_element") map in
                            let pos_ptr = L.build_struct_gep elem_ptr 2 ("elem_pos_ptr") builder in 
                              let x_ptr = L.build_struct_gep pos_ptr 0 ("x_ptr") builder in
                                L.build_store e' x_ptr builder;
                | "y" ->  let elem_ptr = StringMap.find (s1 ^ "_element") map in
                            let pos_ptr = L.build_struct_gep elem_ptr 2 ("elem_pos_ptr") builder in 
                              let y_ptr = L.build_struct_gep pos_ptr 1 ("y_ptr") builder in
                                L.build_store e' y_ptr builder;
              )
            | "size" -> 
              (match x_or_y with
                  | "x" ->  let elem_ptr = StringMap.find (s1 ^ "_element") map in
                              let size_ptr = L.build_struct_gep elem_ptr 2 ("elem_size_ptr") builder in 
                                let x_ptr = L.build_struct_gep size_ptr 0 ("x_ptr") builder in
                                  L.build_store e' x_ptr builder;
                  | "y" -> let elem_ptr = StringMap.find (s1 ^ "_element") map in
                            let size_ptr = L.build_struct_gep elem_ptr 2 ("elem_size_ptr") builder in 
                              let y_ptr = L.build_struct_gep size_ptr 1 ("y_ptr") builder in
                                L.build_store e' y_ptr builder;
              )     
          )


      ) 
  


  in

  (* Invoke "f builder" if the current block doesn't already have a terminal (e.g., a branch). *)
  let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) 
  in

 
  
  (* Build the code for the given statement; return the builder for the statement's successor *)
  let rec stmt builder map = function
        A.Block sl -> List.fold_left (fun builder s -> stmt builder map s) builder sl
      | A.Expr e -> ignore (expr builder map e); builder
      | A.New elem -> 
        let (e_typ, e_typ_check, e_pos) = elem in

        let elem_name = (e_typ ^ "_element") in
        
        (*run the store_elem_func and get the pointer to generic element struct*)
        let store_elem_func = StringMap.find ("store_" ^ e_typ ^ "_element") map in
        let elem_ptr = L.build_call store_elem_func [||] "" builder in

        (* add the new Element position *)
        let pos_ptr = L.build_struct_gep elem_ptr 2 (elem_name ^ "_pos_ptr") builder in
        ignore (L.build_store (expr builder map e_pos) pos_ptr builder); 
  
        (* Call add_element function *)
        ignore (L.build_call add_e [|elem_ptr|] "" builder); builder

      | A.ECall ("add_event", event_name) -> 
        let event = StringMap.find (event_name ^ "_event") events_helper_map in
        
        let condition = string_of_expr event.A.condition in (*condition is now "UP"*)

        let first_elem = List.hd event.A.eformals in (*it's a string. ex player which is now the name*)
        let elem_ptr = StringMap.find (first_elem ^ "_element") map in


        let first_stmt = List.hd event.A.action in
        let event_action_spec = stmt builder map first_stmt in





        (* assume we have keypress-up for now *)
        let event_func_type = L.function_type (L.void_type context) [||] in
        let event_func = L.define_function "event_func" event_func_type the_module in
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
        let not_pressed_bb = L.append_block context ("not_pressed_" ^ condition) event_func in

        let is_pressed_builder = L.builder_at_end context is_pressed_bb in
        (* let elem_ptr = L.build_call delete_elem_func [|elem_name_str_ptr|] "" is_pressed_builder in *)
        ignore(L.build_call delete_elem_func [|elem_name_str_ptr|] "" is_pressed_builder);


        (* let new_pos = L.const_named_struct pair_t [|L.const_int i32_t 300; L.const_int i32_t 300|] in *)



        (* Element position *)
        (* let elem_pos_ptr = L.build_struct_gep elem_ptr 2 ("roy" ^ "_pos_ptr") is_pressed_builder in *)
        (* ignore (L.build_store new_pos elem_pos_ptr is_pressed_builder);  *)


        (* Call add_element function *)
       (*  ignore (L.build_call add_e [|elem_ptr|] "" is_pressed_builder); *)



        ignore (L.build_br not_pressed_bb is_pressed_builder);


        let compare_instruction = L.build_icmp L.Icmp.Eq return_val (L.const_int i32_t 1) ("key_pressed_"^condition) event_builder in
        ignore(L.build_cond_br compare_instruction is_pressed_bb not_pressed_bb event_builder);
   
        ignore (L.build_call c_print_func [||] "" event_builder); (*stored in new basic block*)
      

        let some_builder = L.builder_at_end context not_pressed_bb in
        ignore (L.build_call c_test_func [|event_func|] "" builder); (*giving C the pointer and it will call the func pointer*)
        ignore (L.build_ret_void event_builder); (*need it...not sure why*)
        ignore (L.build_ret_void some_builder); (*???*)
        (* print_test_func new_builder; (*test*) *)
        (* L.const_int i32_t 0 dummy return value *)
        builder (*which builder*)
        
  in




  (* Declare each global variable; remember its value in a map *)
  let global_vars_map =
    let global_var m (t, n, e) =
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
          StringMap.add n local_var map 
        in

        let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
        List.fold_left add_local formals fdecl.A.locals
      in
     map(* build_function_body returns the filled map *)
  in
  List.fold_left build_function_body function_decls_map funcs
in

(* let events_map = 
  let build_event_body m event = 
    let the_event = StringMap.find (event.A.evname ^ "_event") events_helper_map in
    let event_name = event.A.evname in
    StringMap.add (event.A.evname ^ "_event") the_event m 
  in
  List.fold_left build_event_body functions_map events
in
 *)


let elements_map = 
  let build_element_body m element =
    (* let the_element = StringMap.find (element.A.ename ^ "_element") elements_helper_map in *)



    (* ex: player_element *)
    let elem_name = (element.A.ename ^ "_element") in

    (*a function that stores the generic element for later reference*)
    let store_elem_func_type = L.function_type (L.pointer_type elem_t) [||] in
    let store_elem_func = L.define_function ("store_" ^ elem_name) store_elem_func_type the_module in
    let elem_builder = L.builder_at_end context (L.entry_block store_elem_func) in

    (* Element struct pointer *)
    let elem_ptr = L.build_malloc elem_t (elem_name ^ "_ptr") elem_builder in
 
    (* Element name *)
    let elem_name_str_ptr = L.build_global_stringptr element.A.ename (elem_name ^ "_id_str_ptr") elem_builder in
    let elem_name_ptr = L.build_struct_gep elem_ptr 0 (elem_name ^ "_name_ptr") elem_builder in
    ignore (L.build_store elem_name_str_ptr elem_name_ptr elem_builder);
    
    (* Element size *)
    let elem_size_ptr = L.build_struct_gep elem_ptr 1 (elem_name ^ "_size_ptr") elem_builder in
    let size_expr = get_var_expr "size" element.A.e_properties in 
    ignore (L.build_store (expr elem_builder m size_expr) elem_size_ptr elem_builder);

    (* Element color *)
    let color_expr = get_var_expr "color" element.A.e_properties in
    let color_str = string_of_expr color_expr in
    let elem_color_str_ptr = L.build_global_stringptr color_str (elem_name ^ "_color_str_ptr") elem_builder in
    let color_ptr = L.build_struct_gep elem_ptr 3 (elem_name ^ "_color_ptr") elem_builder in
    ignore (L.build_store elem_color_str_ptr color_ptr elem_builder);


    (*need a return value for the builder/func*)
    ignore (L.build_ret elem_ptr elem_builder); (*not sure about this return val*)
    let m = StringMap.add elem_name elem_ptr m in (*store the elem ptr in map*)
    StringMap.add ("store_" ^ elem_name) store_elem_func m (*store ptr to elem store func in map and return this map*)
  in
  List.fold_left build_element_body functions_map elements 
in











 
  (* CREATE WORLD *)
  let world_start_func world map builder =
  
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

    (* World statements *)
    let world_stmt_list = world.A.init_body in
    List.fold_left (fun b s -> stmt b map s) builder world_stmt_list;

    (* ignore (L.build_ret builder); *)
    world_ptr
  in



  (* MAIN FUNCTION *)
  let main_func_type = L.function_type i32_t [||] in
  let main_func = L.define_function "main" main_func_type the_module in
  let main_func_builder = L.builder_at_end context (L.entry_block main_func) in



  let world_ptr = world_start_func world elements_map main_func_builder  in 


  
  ignore (L.build_call init_world_func [|world_ptr|] "" main_func_builder);
  ignore (L.build_call world_func [||] "" main_func_builder);
  
  (* ignore (L.build_call start_render_func [||] "" main_func_builder);  *)
  ignore (L.build_ret (L.const_int i32_t 0) main_func_builder);
  print_map elements_map;

the_module