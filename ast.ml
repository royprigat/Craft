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
  | Pair of int * int
  | Color of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
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

type var_init = SetVar of typ * string * expr

(* World *)
type world = {
    params: expr StringMap;
    body: stmt list;
  }

(* Program *)
type program = world