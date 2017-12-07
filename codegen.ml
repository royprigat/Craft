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

module StringMap = Map.Make(String)

let translate (world) =
  let context = L.global_context () in
  let the_module = L.create_module context "Craft"
  
  and i32_t  = L.i32_type  context (*32 bit int*)
  and i8_t   = L.i8_type   context (*8 bit pointer*)
  and i1_t   = L.i1_type   context (*1 bit int boolean*)
  and float_t = L.float_type context (*float*)
  and void_t = L.void_type context (*void*)
  in
  
  let pair_t = L.named_struct_type context "pair" in 
    L.struct_set_body pair_t [|i32_t; i32_t|] false;
  let color_t = L.named_struct_type context "color" in 
    L.struct_set_body color_t [|i32_t; i32_t; i32_t|] false;
  let world_t = L.named_struct_type context "world" in (*world struct*)
    L.struct_set_body world_t [|color_t; pair_t|] false;
  


(*general note: when declaring structs the naming does not have to match name in c code*)

  let ltype_of_typ = function
      A.Int -> i32_t 
    | A.Float -> float_t
    | A.Bool -> i1_t
    | A.Void -> void_t (*Do we want void type?*)
    | A.Pair -> pair_t
    (* need pair here...? *)
  in 


  (* I don't think we want global variables? Meaning we are not putting any variables outisde the "world" *)

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Declare the world() function *)
  let world_func_t = L.function_type void_t [|L.pointer_type world_t|] in
  let world_func = L.declare_function "world" world_func_t the_module in (*name has to match function name in c code*)

  (*Using this to make a main function since we haven't defined one and it's needed to gcc compilation*)
  let main_func_type = L.function_type i32_t [||] in 
  let main_func = L.define_function "main" main_func_type the_module in




  let var_map vdecl = 

      let builder = L.builder_at_end context (L.entry_block vdecl ) in

  let add_var m (t,s,e) =
    let var = L.build.build_alloca (ltype_of_typ t) (expr builder e) builder
  in StringMap.add (expr builder e) var m 
  in 
  List.iter add_var StringMap.empty world.body;













  (* Construct code for an expression; return its value *)
    let rec expr builder = function
       A.ILiteral i -> L.const_int i32_t i
      | A.BLiteral b -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      (*| A.Id s -> L.build_load (lookup s) s builder*)
      | A.Binop (e1, op, e2) ->
    let e1' = expr builder e1
    and e2' = expr builder e2 in
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
    (* | A.Pair (x,y) ->  *)

    in

    let var_decl_list = world.A.body in

    (* let world_props =
      let world_prop m (t,s,e) =  
        StringMap.add s e m in
      List.fold_left world_prop StringMap.empty var_decl_list in *)


  

 







  (* let vars = function 
    A.SetVar(t,s,e) -> L.const_int i32_t e in *)


  
  (* let d_type A.SetVar(t,n,e) = function -> ltype_of_typ(t) in *)
  (* let d_name = A.SetVAr(t,n,e) -> n in *)
  (* let d_expr = A.SetVar(t,n,e) -> L.const_int i32_t 300 in *)


  (*Need to make a main function since it's required for the c compilation *)
  let main_func_builder = L.builder_at_end context (L.entry_block main_func) in
  
    (* Create a pair struct which will have the screen size hard coded *)
    (* let pair_ptr = L.build_alloca pair_t "test_pair" main_func_builder in *)
    

    let local_pair = L.const_named_struct pair_t [|L.const_int i32_t 1000; L.const_int i32_t 400|] in
      (* let local_pair = L.const_named_struct pair_t [|L.const_int i32_t d_expr; L.const_int i32_t d_expr|] in *)

      

       (*   
          let int_test1 = L.const_int i32_t 1000 in
          let left_ptr = L.build_struct_gep pair_ptr 0 "left" main_func_builder in
            ignore (L.build_store int_test1 left_ptr main_func_builder);

          let int_test2 = L.const_int i32_t 500 in
          let right_ptr = L.build_struct_gep pair_ptr 1 "right" main_func_builder in
             ignore (L.build_store int_test2 right_ptr main_func_builder); *)


    (* Create a world struct and point to the pait struct which will be the screen size *)
    let world_ptr = L.build_alloca world_t "test_world" main_func_builder in
       let world_size_ptr = L.build_struct_gep world_ptr 1 "test_size_member" main_func_builder in
        ignore(L.build_store local_pair world_size_ptr main_func_builder);


  
        L.build_load world_ptr "test_struct" main_func_builder;


        ignore (L.build_call world_func [|world_ptr|] "" main_func_builder); (*Hard coded the calling of world c function*)
        ignore (L.build_ret (L.const_int i32_t 0) main_func_builder); (*hard coding return 0 since main needs to return an int*)


 the_module

(*

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Declare the built-in printbig() function *)
  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
	ignore (L.build_store p local builder);
	StringMap.add n local m in

      let add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
	A.Literal i -> L.const_int i32_t i
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
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
      | A.Unop(op, e) ->
	  let e' = expr builder e in
	  (match op with
	    A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in
	                   ignore (L.build_store e' (lookup s) builder); e'
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder
      | A.Call ("printbig", [e]) ->
	  L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	 let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
	  A.Void -> L.build_ret_void builder
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

      | A.For (e1, e2, e3, body) -> stmt builder
	    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module


*)

