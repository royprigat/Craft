(* CRAFT Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or | Coll
type uop = Neg | Not
type typ = Int | Float | Bool | Void

type bind = typ * string

type expr =
    Literal of int
  | FLiteral of float
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Asgn of string * expr
  | Accs of string * string
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | Condition of stmt * stmt
  | While of expr * stmt


(* Functions *)
type func_decl = {
    typ : typ;
    fname : string;
    formals : typ * string list;
    locals : typ * string list;
    body : stmt list;
  }

(* Elements *)
type elem_decl = {
    ename : string;
    body : stmt list;
  }

(* Events *)
(* type evnt_decl = {
    evname : string;
    args : bind list;
    body : stmt list;
  } *)

(* World/Game *)
type world_decl = {
    init_body : stmt list;
  }

(* Program *)
type program = bind list * func_decl list * elem_decl list * evnt_decl list * world_decl

(* Pretty-printing functions *)

(* let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Coll -> "!!"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | FLiteral(f) -> string_of_float f
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Accs(v, e) -> v ^ "." ^ e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Condition(s1, s2) -> "condition {"

let string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Void -> "void"

let string_of_bind (t,v) = string_of_typ t ^ " " ^ v  

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  "\nfun " ^ string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_evnt  = function
  Event(e,v,a) ->  string_of_eventCheck e ^ " -> {\n" ^
   String.concat "" (List.map string_of_vdecl v) ^
   String.concat "" (List.map string_of_stmt a) ^ "}\n"

let string_of_elemdecl edecl =
  "\nentity " ^ edecl.ename ^ " " ^
  "{\n" ^
  String.concat "" (List.map string_of_vdecl edecl.members) ^ 
  String.concat "" (List.map string_of_event edecl.rules) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^
  String.concat "" (List.map string_of_entdecl ents) *)
