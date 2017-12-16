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



let translate (events, elements, world) =
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
 (*  let s_struct_t = L.named_struct_type context "string_struct" in
  L.struct_set_body s_struct_t [|str_t|] false; *)

  (* Global map of elements  *)
  let fill_elem_map m element = 
    StringMap.add (element.A.ename ^ "_element") element m in
  let elements_map = List.fold_left fill_elem_map StringMap.empty elements in

  (* Global map of events *)
  let fill_event_map m event =
    StringMap.add (event.A.evname ^ "_event") event m in
  let events_map = List.fold_left fill_event_map StringMap.empty events in


  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Float -> flt_t
    | A.Bool -> i1_t
    | A.Void -> void_t
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
(* 
  let print_test_func builder = 
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    ignore (L.build_call printf_func [|int_format_str; (L.const_int i32_t 23)|] "" builder);
  in *)


    (* Construct code for an expression; return its value *)
  let rec expr builder = function
      A.ILiteral i -> L.const_int i32_t i
    (* | A.Fliteral f -> L.const_float flt_t f *)
    | A.SLiteral s -> L.const_string context s
    | A.BLiteral b -> L.const_int i1_t (if b then 1 else 0)
    | A.Noexpr -> L.const_int i32_t 0
    | A.Keypress s -> (expr builder s)
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

    | A.ECall ("add_event", event_name, args) -> 
      let event = StringMap.find (event_name ^ "_event") events_map in
      (* let condition = (expr builder event.A.condition) in (*condition is now "UP"*) *)
      (* let condition = "UP" in  *)

      (* assume we have keypress-up for now *)
      let event_func_type = L.function_type (L.void_type context) [||] in
      let event_func = L.define_function "test_event_func" event_func_type the_module in
      let new_builder = L.builder_at_end context (L.entry_block event_func) in (*event_func runs a new basic block. trigggered by fucntion pointer in C*)
      (* let condition = string_of_expr event.A.condition in (*condition is now "UP"*) *)
      let condition = string_of_expr event.A.condition in (*condition is now "UP"*)
      (* let str_ptr = L.build_alloca str_t "test_str_ptr" new_builder in 
       *)

     (*  let str_ptr = L.build_global_stringptr "UP" ("test_str_ptr") new_builder in *)
      let str_ptr = L.build_global_stringptr condition ("test_str_ptr") new_builder in
      
     
      ignore (L.build_call c_print_func [||] "" new_builder); (*stored in new basic block*)
      
      (* let event_condition_ptr = L.build_global_stringptr "test_string_haha" (event_name ^ "_event_str_ptr") new_builder in (*for testing*) *)
      (* ignore (L.build_store event_condition_ptr string_ptr new_builder); *)
     (*  let s_struct_ptr = L.build_struct_gep  *)
      (* ignore (L.build_call is_key_pressed_func [|int_ptr|] "" new_builder); (*stored in new basic block*) *)
      (* ignore (L.build_call is_key_pressed_func [|(L.const_int i32_t 23)|] "" new_builder); (*stored in new basic block*) *)
      ignore (L.build_call is_key_pressed_func [|str_ptr|] "" new_builder); (*stored in new basic block*)



      ignore (L.build_call c_test_func [|event_func|] "" builder); (*giving C the pointer and it will call the func pointer*)
  
      ignore (L.build_ret_void new_builder); (*need it...not sure why*)
      (* print_test_func new_builder; (*test*) *)
      L.const_int i32_t 0 (*dummy return value*)








    (* | A.Assign (s, e) -> let e' = expr builder e in
      ignore (L.build_store e' (lookup s) builder); e' *)
    (* | A.Call ("add_event", [e1,e2]) -> *)

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

  (* Store all elements struct pointers in main map *)
  (* let store_elements m elem_type elem_n builder =
    let store_element m element =
      (* if elem_type = element.A.ename then *)
       let elem_name = (elem_n ^ "_" ^ element.A.ename ^ "_element") in
       let elem_ptr = L.build_malloc elem_t (elem_name ^ "_ptr") builder in 
        
       let elem_size_ptr = L.build_struct_gep elem_ptr 0 (elem_name ^ "_size_ptr") builder in
       let size_expr = get_var_expr "size" element.A.properties in 
       ignore (L.build_store (expr builder size_expr) elem_size_ptr builder);
  
       let color_expr = get_var_expr "color" element.A.properties in
       let color_str = string_of_expr color_expr in
       let elem_color_str_ptr = L.build_global_stringptr color_str (elem_name ^ "_color_str_ptr") builder in
       let color_ptr = L.build_struct_gep elem_ptr 2 (elem_name ^ "_color_ptr") builder in
       ignore (L.build_store elem_color_str_ptr color_ptr builder);
       StringMap.add elem_name elem_ptr m
       in
   
    List.fold_left store_element m elements
  in *)
  
  (* Build the code for the given statement; return the builder for the statement's successor *)
  let rec stmt builder = function
        A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.New elem -> 
        let (e_typ, e_id, e_typ_check, e_pos) = elem in
        let element = StringMap.find (e_typ ^ "_element") elements_map in

        (* Element struct pointer *)
        let elem_name = (e_id ^ "_" ^ element.A.ename ^ "_element") in
        let elem_ptr = L.build_malloc elem_t (elem_name ^ "_ptr") builder in
        
        (* Element ID *)
        let elem_id_str_ptr = L.build_global_stringptr e_id (elem_name ^ "_id_str_ptr") builder in
        let id_ptr = L.build_struct_gep elem_ptr 0 (elem_name ^ "_id_ptr") builder in
        ignore (L.build_store elem_id_str_ptr id_ptr builder);
        
        (* Element size *)
        let elem_size_ptr = L.build_struct_gep elem_ptr 1 (elem_name ^ "_size_ptr") builder in
        let size_expr = get_var_expr "size" element.A.e_properties in 
        ignore (L.build_store (expr builder size_expr) elem_size_ptr builder);

        (* Element position *)
        let elem_pos_ptr = L.build_struct_gep elem_ptr 2 (elem_name ^ "_pos_ptr") builder in
        ignore (L.build_store (expr builder e_pos) elem_pos_ptr builder); 
  
        (* Element color *)
        let color_expr = get_var_expr "color" element.A.e_properties in
        let color_str = string_of_expr color_expr in
        let elem_color_str_ptr = L.build_global_stringptr color_str (elem_name ^ "_color_str_ptr") builder in
        let color_ptr = L.build_struct_gep elem_ptr 3 (elem_name ^ "_color_ptr") builder in
        ignore (L.build_store elem_color_str_ptr color_ptr builder);

        (* Call add_element function *)
        ignore (L.build_call add_e [|elem_ptr|] "" builder); builder
        
  in

  (* Build the code for each statement in the function *)
  (* let builder = stmt builder (A.Block fdecl.A.body) in *)
    (* Add a return if the last block falls off the end *)
    (* add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in *)



  let store_props m props builder =
    let store_prop m (t,n,e) = 
    let e' = expr builder e in 
    let prop = L.build_alloca (ltype_of_typ t) n builder in
    ignore (L.build_store e' prop builder); 
    StringMap.add n prop m in

    List.fold_left store_prop m props
  in

 
  (* CREATE WORLD *)
  let world_start_func world builder =
  
    let world_ptr = L.build_malloc world_t ("world_ptr") builder in 
    
    (* World size struct and pointer *)
    let world_size_ptr = L.build_struct_gep world_ptr 0 ("size_ptr") builder in
    let size_expr = get_var_expr "size" world.A.w_properties in 
    ignore (L.build_store (expr builder size_expr) world_size_ptr builder);

    (* World color struct and pointer *)
    let color_expr = get_var_expr "color" world.A.w_properties in
    let color_str = string_of_expr color_expr in
    let world_color_str_ptr = L.build_global_stringptr color_str "color_str_ptr" builder in
    let color_ptr = L.build_struct_gep world_ptr 1 "color_ptr" builder in
    ignore (L.build_store world_color_str_ptr color_ptr builder);

    (* World statements *)
    let world_stmt_list = world.A.init_body in
    List.fold_left stmt builder world_stmt_list;

    (* ignore (L.build_ret world_ptr builder); *)
    world_ptr
  in
  

  (* MAIN FUNCTION *)
  let main_func_type = L.function_type i32_t [||] in
  let main_func = L.define_function "main" main_func_type the_module in
  let main_func_builder = L.builder_at_end context (L.entry_block main_func) in
  
  let world_ptr = world_start_func world main_func_builder in 
  
  ignore (L.build_call init_world_func [|world_ptr|] "" main_func_builder);
  ignore (L.build_call world_func [||] "" main_func_builder);
  
  (* ignore (L.build_call start_render_func [||] "" main_func_builder);  *)
  ignore (L.build_ret (L.const_int i32_t 0) main_func_builder);


the_module