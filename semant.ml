(* Semantic checking for the CRAFT compiler *)

open Ast

module E = Exceptions
module StringMap = Map.Make(String)

let check (globals, funcs, events, elements, world) =

(* HELPERS *)

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
      n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
      in helper (List.sort compare list)
  in

  (* get variable name *)
  let varName = function (_, n, _) -> n in

  (* get variable name *)
  let bindName = function (_, n) -> n in

  (* Raise an exception of the given rvalue type cannot be assigned to
       the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet == rvaluet then lvaluet else raise err
  in

  (* Raise an exception if a given binding is to a void type *)
  (* let check_not_void exceptf = function
      (Void, n, e) -> raise (Failure (exceptf n))
    | _ -> ()
  in *)

  let string_of_typ = function
    Int -> "int"
    | Float -> "float"
    | Bool -> "bool"
    | Void -> "void"
    | Pair -> "pair"
    | Color -> "color"
  in

  (**** Checking Global Variables ****)

  (* List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals; *)

  report_duplicate (fun n -> "duplicate global " ^ n) (List.map varName globals);

(* CHECK FUNCTIONS *)
  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) funcs);

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                           StringMap.empty funcs
  in

  let function_decl s = try StringMap.find s function_decls
         with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let check_function func =

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map bindName func.formals);

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map varName func.locals);

  (* Type of each variable (global, formal, or local *)
  let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
  	StringMap.empty (globals @ func.formals @ func.locals )
  in

  (* return the type of an ID (check given symbols map) *)
  let type_of_identifier s =
      try StringMap.find s symbols
        with Not_found -> raise (E.UndeclaredId(s))
    in

  (* Return the type of an expression or throw an exception *)
  let rec expr m arg = function
	   ILiteral _ -> Int
     | BLiteral _ -> Bool
     | FLiteral _ -> Float
     | Id s -> type_of_identifier s
     | Binop(e1, op, e2) as e -> let t1 = expr m arg e1 and t2 = expr m arg e2 in
	    (match op with
        Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
        |  Add | Sub | Mult | Div when t1 = Int && t2 = Float -> Float
		    |  Add | Sub | Mult | Div when t1 = Float && t2 = Int -> Float
   	    |  Add | Sub | Mult | Div when t1 = Float && t2 = Float -> Float
	      | Equal | Neq when t1 = t2 -> Bool
	      | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
	      | Less | Leq | Greater | Geq when t1 = Float && t2 = Float -> Bool
        | Less | Leq | Greater | Geq when t1 = Float && t2 = Int -> Bool
        | Less | Leq | Greater | Geq when t1 = Int && t2 = Float -> Bool
	      | And | Or when t1 = Bool && t2 = Bool -> Bool
        | _ -> raise (E.IllegalBinOp(string_of_typ t1 ^ " ", string_of_op op ^ " ",
        string_of_typ t2 ^ " in ", string_of_expr e))
      )
    | Unop(op, e) as ex -> let t = expr m arg e in
	   (match op with
	      Neg when t = Int -> Int
	       | Not when t = Bool -> Bool
         | _ -> raise (E.IllegalUnOp(string_of_uop op, string_of_typ t ^ " in ",
          string_of_expr ex))
     )
    | Noexpr -> Void
    (* | Assign(var, e) as ex -> let lt = type_of_identifier var m
                              and rt = expr m e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				        " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex)) *)

    (* | Call(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun (ft, _) e -> let et = expr e in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
           fd.typ*)

    (* | Pr(e1,e2) as e -> let t1 = expr m e1 and t2 = expr m e2 in
      (match t1 with
          Int when t2 = Int -> Pair
          | _ -> raise (E.IllegalPairType("A pair must be made up of two ints"))
      ) *)
    in

    in
    (* Verify a statement or throw an exception *)
    let rec stmt m arg = function
    	Block sl -> let rec check_block = function
        [Return _ as s] -> stmt m arg s
          | Return _ :: _ -> raise (Failure "nothing may follow a return")
          | Block sl :: ss -> check_block (sl @ ss)
          | s :: ss -> stmt m arg s ; check_block ss
          | [] -> ()
            in check_block sl
        | Expr e -> ignore (expr m arg e)
        (*)| Return e -> let t = expr m arg e in if t = funcs.typ then () else
             raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                             string_of_typ func.typ ^ " in " ^ string_of_expr e))
         | If(p, b1, b2) -> check_bool_expr p m arg; stmt m arg b1; stmt m arg b2
        | While(p, s) -> check_bool_expr m arg p; stmt m arg s
        | Return e -> ()*)
        in



(* check correct type assignment for "size" *)
(*  let check_type s t m err =
    if lvalue == "size" && rvaluet == Pair then lvalue else raise err
  in*)

  (* build a map given a list of members *)
  let memMap members =
    List.fold_left (fun m (t, n, e) -> StringMap.add n t m) StringMap.empty members
  in

  (* check if a given member type exists *)
  let exist s t m =
    if not(StringMap.mem s m) then raise (E.UndefinedId(s))
    else
    let myMem = StringMap.find s m in
    if myMem != t then raise (E.IncorrectArgumentType("expected: " ^ string_of_typ t,
                              "found: " ^ string_of_typ myMem))



  (*  try
      let typeT = StringMap.find s m
      in
      if typeT != t then raise (Failure ("Wrong type for " ^ s))
      with Not_found -> raise (Failure ("You haven't defined " ^ s))
    in*)

  (* add variable to a map *)
let var m (t, n, e) = StringMap.add n t m in

(* CHECK ELEMENTS *)

let check_elements el =
  (* check required properties *)
  print_string("check");
   let elMems = memMap el.e_properties in
        exist "color" Color elMems;
        exist "size" Pair elMems;

 report_duplicate (fun n -> "Duplicate variable <" ^ n ^ "> in your element properties")
   (List.map varName el.e_properties);

in

List.iter check_elements elements;

 (*report_duplicate (fun n -> "Duplicate local variable <" ^ n ^ "> in your world")
   (List.map varName world.init_locals);*)


(*let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                    built_in_decls functions
in

let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
in*)


(*  CHECK WORLD *)
  let check_world w =

    (* check required properties *)
    let wMems = memMap w.w_properties in
          exist "color" Color wMems;
          exist "size" Pair wMems;
    in
    (* let mems = memTypes world.properties in *)

        (* build a map of variables within scope *)
        let scopeMap =  List.fold_left var StringMap.empty world.w_properties in

        (* check members *)
      (*  List.iter (checkVarInit symbols "" ) world.properties;
*)
        (* check for duplicate world properties *)
        report_duplicate (fun n -> "Duplicate variable <" ^ n ^ "> in your world properties")
          (List.map varName world.w_properties);

        report_duplicate (fun n -> "Duplicate local variable <" ^ n ^ "> in your world")
          (List.map varName world.init_locals);

(*

        (* check init members and add to symbols list *)
        let symbols =  List.fold_left var symbols world.init_locals in
        List.iter (checkVarInit symbols "") world.init_locals;

        (* check init body *)
        stmt symbols "" (Block world.init_body);
      in
*)
    check_world world;
