(* Semantic checking for the CRAFT compiler *)

open Ast

module E = Exceptions
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

  let string_of_typ = function
    Int -> "int"
    | Float -> "float"
    | Bool -> "bool"
    | Void -> "void"
    | Pair -> "pair"
    | Color -> "color"
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
  in



  (*  try
      let typeT = StringMap.find s m
      in
      if typeT != t then raise (Failure ("Wrong type for " ^ s))
      with Not_found -> raise (Failure ("You haven't defined " ^ s))
    in*)

  (* get variable name *)
let varName = function (_, n, _) -> n in

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
(*
        (* build a symbols map - variables within scope *)
        let symbols =  List.fold_left var StringMap.empty world.properties in

        (* check members *)
        List.iter (checkVarInit symbols "" ) world.properties;
*)
        (* check for duplicate world properties *)
        report_duplicate (fun n -> "Duplicate variable <" ^ n ^ "> in your world properties")
          (List.map varName world.w_properties);

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
