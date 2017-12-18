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

  (* get string name from list *)
  let strName = function (n) -> n in

  (* get variable name *)
  let varName = function (_, n, _) -> n in

  (* get variable name *)
  let bindName = function (_, n) -> n in

  (* get elements from function and add to list*)
  let rec insert x = function
     | [] -> [x]
     | h :: t as l -> x :: l
  in

  (* Raise an exception of the given rvalue type cannot be assigned to
       the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet == rvaluet then lvaluet else raise err
  in

  let string_of_typ = function
    Int -> "int"
    | Float -> "float"
    | Bool -> "bool"
    | Void -> "void"
    | Pair -> "pair"
    | Color -> "color"
  in

let globalsList = List.map varName globals in
let wlocalsList = List.map varName world.init_locals in
let bigList = globalsList @ wlocalsList in

let func_create func =
  insert func.fname bigList;
  bigList = (List.map bindName func.formals) @ (List.map varName func.locals) @ bigList;
in
List.iter func_create funcs;

let evnt_create evnt =
  insert evnt.evname bigList;
in
List.iter evnt_create events;

let elmnt_create elmnt =
  insert elmnt.ename bigList;
in
List.iter elmnt_create elements;

report_duplicate (fun n -> "duplicate names " ^ n) bigList;

(*
let check_function func =

  report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map bindName func.formals);

  report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map varName func.locals);

  insert func.fname bigList;
 *)


  (* let symbol = List.fold_left (fun m (t, n) -> StringMap.add n t m)
    symbol func.formals
  in *)

  (* Type of each variable locals *)
  (* let symbol = List.fold_left (fun m (t, n, e) -> StringMap.add n t m)
    symbol func.locals
  in *)

  (* stmt (Block func.body); *)

  (* in
  List.iter check_function funcs; *)




  (**** Checking Global Variables ****)
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map varName globals);

  (* CHECK FUNCTIONS *)
    report_duplicate (fun n -> "duplicate function " ^ n)
      (List.map (fun fd -> fd.fname) funcs);

    let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                             StringMap.empty funcs in

    let function_decl s = try StringMap.find s function_decls
           with Not_found -> raise (E.UnrecognizedFunction(s)) in

   (* Type of each variable (global or formal *)
    let symbol = List.fold_left (fun m (t, n, e) -> StringMap.add n t m)
      StringMap.empty globals
    in


    (* return the type of an ID (check given symbols map) *)
    let type_of_identifier s =
        try StringMap.find s symbol
        with Not_found -> raise (E.UndeclaredId(s))
    in


   (* Return the type of an expression or throw an exception *)
     let rec expr = function
      ILiteral _ -> Int
      | BLiteral _ -> Bool
      | FLiteral _ -> Float
      | Id s -> type_of_identifier s
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
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
     | Unop(op, e) as ex -> let t = expr e in
      (match op with
         Neg when t = Int -> Int
          | Not when t = Bool -> Bool
          | _ -> raise (E.IllegalUnOp(string_of_uop op, string_of_typ t ^ " in ",
           string_of_expr ex))
      )
     | Noexpr-> Void
     | Assign(var, e) as ex -> let lt = expr var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				     " = " ^ string_of_typ rt ^ " in " ^
				     string_of_expr ex))

     in

     (* Verify a statement or throw an exception *)
     let rec stmt = function
       Block sl -> let rec check_block = function
         [Return _ as s] -> stmt s
           | Return _ :: _ -> raise (E.PassedReturn("nothing may follow a return statement"))
           | Block sl :: ss -> check_block (sl @ ss)
           | s :: ss -> stmt s ; check_block ss
           | [] -> ()
             in check_block sl
         | Expr e -> ignore (expr e)

     in

    (* let check_function func =

      report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
        (List.map bindName func.formals);

      report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
        (List.map varName func.locals);

    let symbol = List.fold_left (fun m (t, n) -> StringMap.add n t m)
      symbol func.formals
    in

    (* Type of each variable locals *)
    let symbol = List.fold_left (fun m (t, n, e) -> StringMap.add n t m)
      symbol func.locals
    in

  stmt (Block func.body);

  in
  List.iter check_function funcs; *)


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
  in


  (* add variable to a map *)
let addVar m (t, n, e) = StringMap.add n t m in

(* check if types in var_decl match *)
(* let checkVars m = function
(t,n,e) -> let ty = expr m e in
if t = ty
  then ()
   else raise (E.IncorrectType("expected type: " ^ string_of_typ t, ", not " ^ string_of_expr e, " of type: " ^ string_of_typ ty))
in *)

(* CHECK ELEMENTS *)

let check_elements el =
  (* check required properties *)
   let elMems = memMap el.e_properties in
        exist "color" Color elMems;
        exist "size" Pair elMems;

 report_duplicate (fun n -> "Duplicate variable <" ^ n ^ "> in your element properties")
   (List.map varName el.e_properties);

in

List.iter check_elements elements;



(*  CHECK WORLD *)
  let check_world w =

    (* check required properties *)
    let wMems = memMap w.w_properties in
          exist "color" Color wMems;
          exist "size" Pair wMems;

    (* build a map of variables within scope *)
    (* let symbol = List.fold_left addVar symbol w.w_properties in

    let symbol = List.fold_left addVar symbol w.init_locals in *)

    (* check for duplicate world properties *)
    report_duplicate (fun n -> "Duplicate variable <" ^ n ^ "> in your world properties")
      (List.map varName w.w_properties);

    report_duplicate (fun n -> "Duplicate local variable <" ^ n ^ "> in your world")
      (List.map varName w.init_locals);

      (* check members *)
    (* List.iter (checkVars bothSymbols ) w.w_properties; *)

    (* check world body *)
    stmt (Block w.init_body);

    in
    check_world world;
