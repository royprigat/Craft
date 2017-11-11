(* CRAFT Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or | Coll
type uop = Neg | Not
type typ = Int | Float | Bool | Void | Pair | Color

type bind = typ * string

type expr =
    Lit of int
  | FLit of float
  | BLit of bool
  | Pair of int * int
  | Color of string
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
(* type func_decl = {
    typ : typ;
    fname : string;
    formals : typ * string list;
    locals : typ * string list;
    body : stmt list;
  } *)

(* Elements *)
type element = {
    name : string;
    body : stmt list;
  }

(* Events *)
(* type evnt_decl = {
    evname : string;
    args : bind list;
    body : stmt list;
  } *)

(* World *)
type world = {
    body : stmt list;
  }

(* Program *)
type program = element list * world