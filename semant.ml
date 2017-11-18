(* Semantic checking for the CRAFT compiler *)

open Ast

module E = Exceptions
module StringMap = Map.Make(String)


let check (gboard) = 

  (* check for duplicates within a list *)
  let find_duplicate exceptf list =
    let rec helper = function
       n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* check if given type is an int or float *)
  let get_num_type t = if (t = Int || t = Float) then true else false in

  (* extract variable name from varinit *)
  let get_var_name = function SetVar(_, n, _) -> n in

  (* Raise an exception for type assignment mismatch *)
  let check_assign lvaluet rvaluet err =
    if lvaluet == rvaluet then lvaluet else raise err in








(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)



(* Raise an exception if the given list has a duplicate *)





(* Raise an exception if a given binding is to a void type *)





(* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)





(**** Checking Global Variables ****)





(**** Checking Functions ****)





(* Function declaration for a named function *)



(* Type of each variable (global, formal, or local *)


(* Return the type of an expression or throw an exception *)




(* Verify a statement or throw an exception *)

