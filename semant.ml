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

  let check_assign lvaluet rvaluet =
    if lvaluet == rvaluet then lvaluet else raise (E.InvalidAssignment)
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

  let check_world world = 
    
    (* check required properties *)
    let myMems = memTypes gb.gmembers in 
    checkMemExists "clr" Color myMems;
    checkMemExists "size" Vector myMems;
    
        (* build a symbols map - variables within scope *)
        let symbols =  List.fold_left var StringMap.empty gb.gmembers in
    
        (* check members *)
        List.iter (checkVarInit symbols "" ) gb.gmembers;
    
        (* check for duplicate init members *)
        reportDuplicate (fun n -> "Duplicate init function member " ^ n ^ " in " ^ gb.gname)
          (List.map varDeclName gb.init_mem);
    
        (* check init members and add to symbols list *)
        let symbols =  List.fold_left var symbols gb.init_mem in
        List.iter (checkVarInit symbols "") gb.init_mem;
    
        (* check init body *)
        stmt symbols "" (Block gb.init_body);
      in 

    check_world world;
