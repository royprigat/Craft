(* Semantic checking for the CRAFT compiler *)

open Ast

(* module E = Exceptions *)
module StringMap = Map.Make(String)

let check (events, elements, world) =

(* HELPERS *)

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
      n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
      in helper (List.sort compare list)
  in

  (* Raise an exception of the given rvalue type cannot be assigned to
       the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet == rvaluet then lvaluet else raise err
  in

  (* build a map given a list of members *)
  let memMap members =
    List.fold_left (fun m (t, n, e) -> StringMap.add n t m) StringMap.empty members
  in

  (* check if a given member type exists *)
  let exist s t m =
    try
      let myT = StringMap.find s m
      in
      if myT != t then raise (Failure ("Inconsistent types"))
      with Not_found -> raise (Failure ("You haven't defined " ^ s))
    in

  (* get variable name *)
let varName = function (_, n, _) -> n in

(*
(* CHECK ELEMENTS *)
let check_elements elements =

  (* check required properties *)
  let elMems = memMap elements.properties in
        exist "color" Color elMems;
        exist "size" Pair elMems;
  in

check_elements elements;


report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd elements); *)


(*let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                    built_in_decls functions
in

let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
in*)


(*  CHECK WORLD *)
  let check_world world =

    (* check required properties *)
    let wMems = memMap world.properties in
          exist "color" Color wMems;
          exist "size" Pair wMems;
    in
    (* let mems = memTypes world.properties in *)
(*
        (* build a symbols map - variables within scope *)
        let symbols =  List.fold_left var StringMap.empty world.properties in

        (* check members *)
        List.iter (checkVarInit symbols "" ) world.properties;
*)
        (* check for duplicate world properties *)
        report_duplicate (fun n -> "Duplicate properties member <" ^ n ^ "> in your world")
          (List.map varName world.properties);

        report_duplicate (fun n -> "Duplicate local variable <" ^ n ^ "> in your world")
          (List.map varName world.init_locals);

(*
        (* check for correct type assignment in world *)
        check_assign ()

        (* check init members and add to symbols list *)
        let symbols =  List.fold_left var symbols world.init_locals in
        List.iter (checkVarInit symbols "") world.init_locals;

        (* check init body *)
        stmt symbols "" (Block world.init_body);
      in
*)
    check_world world;
