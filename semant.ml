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

  let string_of_typ = function
    Int -> "int"
    | Float -> "float"
    | Bool -> "bool"
    | Void -> "void"
    | Pair -> "pair"
    | Color -> "color"
    | String -> "string"
  in

  let rec string_of_expr = function
    ILiteral(l) -> string_of_int l
  | FLiteral(l) -> string_of_float l
  | BLiteral(true) -> "true"
  | BLiteral(false) -> "false"
  | SLiteral(s) -> s
  | Pr(x,y) -> "(" ^ string_of_expr x ^ "," ^ string_of_expr y ^ ")"
  | Cr(c) -> string_of_expr c
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
    f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | PAccess(s1,s2,e) -> s1 ^ "." ^ s2 ^ "." ^ e
  | CAccess(s1,s2) -> s1 ^ "." ^ s2
  | Keypress(e) -> "key_press(" ^ string_of_expr e ^ ")"
  | Noexpr -> ""

  in

  (**** Checking Global Variables ****)
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map varName globals);

  (* Type of each variable (global or formal *)
   let symbol = List.fold_left (fun m (t, n, e) -> StringMap.add n t m)
     StringMap.empty globals
   in

  (* CHECK FUNCTIONS *)
    report_duplicate (fun n -> "duplicate function " ^ n)
      (List.map (fun fd -> fd.fname) funcs);

   let built_in_decls =  StringMap.add "print"
    { typ = Int; fname = "print"; formals = [(Int, "x")];
      locals = []; body = [] }
      (StringMap.singleton "add"
    { typ = Int; fname = "add"; formals = [(Bool, "e") ; (Pair, "pos")];
      locals = []; body = [] })
   in

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                     built_in_decls funcs in

  let function_decl s = try StringMap.find s function_decls
           with Not_found -> raise (E.UnrecognizedFunction(s)) in


    (* return the type of an ID (check symbols map) *)
    let type_of_identifier s m =
        try StringMap.find s m
        with Not_found -> raise (E.UndeclaredId(s))
    in


   (* Return the type of an expression or throw an exception *)
     let rec expr m = function
      ILiteral _ -> Int
      | BLiteral _ -> Bool
      | FLiteral _ -> Float
      | SLiteral s -> Color
      | Id s -> type_of_identifier s m
      | Binop(e1, op, e2) as e -> let t1 = expr m e1 and t2 = expr m e2 in
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
         | _ -> raise (E.IllegalBinOp("<" ^ string_of_typ t1 ^ "> " ^ string_of_op op ^ " <" ^
         string_of_typ t2 ^ "> in " ^ string_of_expr e))
       )
     | Unop(op, e) as ex -> let t = expr m e in
      (match op with
         Neg when t = Int -> Int
          | Not when t = Bool -> Bool
          | _ -> raise (E.IllegalUnOp(string_of_uop op, string_of_typ t ^ " in ",
           string_of_expr ex))
      )
     | Noexpr-> Void
     | Assign(var, e) as ex -> let lt = expr m var
                                and rt = expr m e in
        check_assign lt rt (Failure ("illegal assignment <" ^ string_of_typ lt ^
				     "> = <" ^ string_of_typ rt ^ "> in " ^
				     string_of_expr ex))
     (* | Cr s -> let c = expr m s in
       let c1 = string_of_typ c in
       if c1 = "color" then Color
       else raise (E.IncorrectColorType("Please enter a 6-digit hex number for color")) *)
     | Pr(x,y) -> let x1 = expr m x and y1 = expr m y in
       if (x1 = Int && y1 = Int) then Pair
       else raise (E.IncorrectPairType("Please enter int inputs for type pair"))
     (* | Call(fname, actuals) as call -> let fd = function_decl fname in
              if List.length actuals != List.length fd.formals then
                raise (Failure ("expecting " ^ string_of_int
                (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
              else
              List.iter2 (fun (ft, _) e -> let et = expr e in
                ignore(check_assign ft et
                    (Failure ("illegal actual argument found " ^ string_of_typ et ^
                    " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
                fd.formals actuals;
              fd.typ
     | PAccess (_, _, _) ->
     | CAccess (_, _) ->
     | Keypress _) ->
     |Call (_, _) ->
      in*)


     in

     let checkBools e m = if expr m e != Bool
            then raise (E.NonBooleanType)
            else ()
       in

     (* Verify a statement or throw an exception *)
     let rec stmt m = function
       Block sl -> let rec check_block = function
         [Return _ as s] -> stmt m s
           | Return _ :: _ -> raise (E.PassedReturn("nothing may follow a return statement"))
           | Block sl :: ss -> check_block (sl @ ss)
           | s :: ss -> stmt m s ; check_block ss
           | [] -> ()
             in check_block sl
         | Expr e -> ignore (expr m e)
         | Return _ -> ()
         |If (c, p1, p2) -> checkBools c m ; stmt m p1; stmt m p2
         |While (c, s) -> checkBools c m ; stmt m s
         (* |Condition (_, _) ->
         |New _ ->
         |ECall (_, _) ->  *)

     in

  let check_function func =

  report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
    (List.map bindName func.formals);

  let symbol = List.fold_left (fun m (t, n) -> StringMap.add n t m)
  symbol func.formals
  in

  report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
    (List.map varName func.locals);

  (* Type of each variable locals *)
  let symbol = List.fold_left (fun m (t, n, e) -> StringMap.add n t m)
  symbol func.locals
  in

  stmt symbol (Block func.body)

  in
  List.iter check_function funcs;


  (* build a map given a list of members *)
  let memMap members =
    List.fold_left (fun m (t, n, e) -> StringMap.add n t m) StringMap.empty members
  in

  (* check if a given member type exists *)
  let exist s t m =
    if not(StringMap.mem s m) then raise (E.UndefinedId(s))
    else
    let myMem = StringMap.find s m in
    if myMem != t then raise (E.IncorrectArgumentType("expected: <" ^ string_of_typ t ^
                              "> found: <" ^ string_of_typ myMem ^ ">"))
  in


  (* add variable to a map *)
let addVar m (t, n, e) = StringMap.add n t m in

(* check if types in var_decl match *)
let checkVars m = function
(t,n,e) -> let ty = expr m e in
if t != ty
  then raise (E.IncorrectType("expected type:<" ^ string_of_typ t ^ "> not " ^ string_of_expr e ^ " of type: " ^ string_of_typ ty))
in

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

    (* check for duplicate world properties *)
    report_duplicate (fun n -> "Duplicate variable <" ^ n ^ "> in your world properties")
      (List.map varName w.w_properties);

    let symbol = List.fold_left addVar StringMap.empty w.w_properties in
    List.iter (checkVars symbol) w.w_properties;

    report_duplicate (fun n -> "Duplicate local variable <" ^ n ^ "> in your world")
      (List.map varName w.init_locals);

    let symbol = List.fold_left addVar symbol w.init_locals in
    List.iter (checkVars symbol) w.init_locals;

    (* check world body *)
    stmt symbol (Block w.init_body);

    in
    check_world world;
