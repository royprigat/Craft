(* Semantic checking for the CRAFT compiler *)

open Ast

module E = Exceptions
module StringMap = Map.Make(String)

let check (world) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate list =
    let rec helper = function
      n1 :: n2 :: _ when n1 = n2 -> raise (E.DuplicateInList n1)
      | _ :: t -> helper t
      | [] -> ()
      in helper (List.sort compare list)
  in

(*  let check_assign lvaluet rvaluet =
    if lvaluet == rvaluet then lvaluet else raise (E.InvalidAssignment)
  in*)

(*  need to add a map of elemts to check expression? *)
  let rec expr = function
  	 Literal _ -> Int
    | BoolLit _ -> Bool
    | FLiteral _ -> Float
    | Id s -> type_of_identifier s
    | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
  	(match op with
    Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
  	| Equal | Neq when t1 = t2 -> Bool
  	| Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
  	| And | Or when t1 = Bool && t2 = Bool -> Bool
    | _ -> raise (Failure ("illegal binary operator " ^
      string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
      string_of_typ t2 ^ " in " ^ string_of_expr e))
      )
    | Unop(op, e) as ex -> let t = expr e in
  	 (match op with
  	  Neg when t = Int -> Int
    | Not when t = Bool -> Bool
    | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
  	   string_of_typ t ^ " in " ^ string_of_expr ex)))
    | Noexpr -> Void
    | Assign(var, e) as ex -> let lt = type_of_identifier var and rt = expr e in
      check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
  	  " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex))
    | Call(fname, actuals) as call -> let fd = function_decl fname in
      if List.length actuals != List.length fd.formals then
      raise (Failure ("expecting " ^ string_of_int
      (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
      else
      List.iter2 (fun (ft, _) e -> let et = expr e in
      ignore (check_assign ft et
      (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
            fd.formals actuals;
            fd.typ
    in


  (* Verify a statement or throw an exception *)
  let rec stmt = function
	  Block sl -> let rec check_block = function
      [Return _ as s] -> stmt s
    | Return _ :: _ -> raise (E.NoReturnValue)
    | Block sl :: ss -> check_block (sl @ ss)
    | s :: ss -> stmt s ; check_block ss
    | [] -> ()
        in check_block sl
    | Expr e -> ignore (expr e)
    | Return e -> let t = expr e in if t = func.typ then () else
        raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
        string_of_typ func.typ ^ " in " ^ string_of_expr e))

    | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
    | While(p, s) -> check_bool_expr p; stmt s
  in


  (* check if a given member type exists *)
  let checkExist str ty m =
    try
      let myT = StringMap.find str m
      in
      if myT != ty then raise (Failure ("Inconsistent types"))
     with Not_found -> raise (Failure ("You haven't defined " ^ str))
  in

(*  CHECK WORLD *)
  let check_world world =

    (* check required properties *)
    let mems = memTypes world.properties in
    checkExist "clr" Color mems;
    checkExist "size" Pair mems;

        (* build a symbols map - variables within scope *)
        let symbols =  List.fold_left var StringMap.empty world.properties in

        (* check members *)
        List.iter (checkVarInit symbols "" ) world.properties;

        (* check for duplicate init members *)
        reportDuplicate (fun n -> "Duplicate init function member " ^ n ^ " in your world")
          (List.map varDeclName world.init_locals);

        (* check init members and add to symbols list *)
        let symbols =  List.fold_left var symbols world.init_locals in
        List.iter (checkVarInit symbols "") world.init_locals;

        (* check init body *)
        stmt symbols "" (Block world.init_body);
      in

    check_world world;
