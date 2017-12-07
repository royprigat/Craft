(* CRAFT Abstract Syntax Tree *)

module StringMap = Map.Make(String)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or
type uop = Neg | Not
type typ = Int | Float | Bool | Void | Pair | Color

type expr =
    ILiteral of int
  | FLiteral of float
  | SLiteral of string
  | BLiteral of bool
  | Pair of expr * expr
  | Color of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | Access of string * string
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | Condition of stmt * stmt
  | While of expr * stmt

type var_decl = typ * string * expr

(* World *)
type world = {
  body: var_decl list;
}

(* Program *)
type program = world




(* AST Print functions *)

let string_of_typ = function
  Int -> "int"
| Float -> "float"
| Bool -> "bool"
| Void -> "void"
| Pair -> "pair"
| Color -> "color"

let string_of_op = function
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

let string_of_uop = function
  Neg -> "-"
| Not -> "!"


let rec string_of_expr = function
  ILiteral(l) -> string_of_int l
| FLiteral(l) -> string_of_float l
| BLiteral(true) -> "true"
| BLiteral(false) -> "false"
| SLiteral(s) -> s
| Pair(x,y) -> "(" ^ string_of_expr x ^ "," ^ string_of_expr y ^ ")"
| Color(c) -> c
| Id(s) -> s
| Binop(e1, o, e2) ->
  string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
| Unop(o, e) -> string_of_uop o ^ string_of_expr e
| Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
| Call(f, el) ->
  f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
| Noexpr -> ""


let string_of_vars = function  
  (t,s,e) -> string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n"

let string_of_world this_world =
  List.map string_of_vars this_world.body

let string_of_program (world) =
  String.concat "" (string_of_world world)