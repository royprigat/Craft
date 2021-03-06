(* CRAFT Abstract Syntax Tree *)

module StringMap = Map.Make(String)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or
type uop = Neg | Not
type typ = Int | Float | Bool | Void | Pair | Color | String
type bind = typ * string

type expr =
    ILiteral of int
  | FLiteral of float
  | SLiteral of string
  | BLiteral of bool
  | Pr of expr * expr
  | Cr of expr
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | PAccess of string * string * string
  | CAccess of string * string
  | Keypress of expr
  | Call of string * expr list
  | Noexpr

type element_decl = string * string * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | Condition of stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt
  | New of element_decl
  | ECall of string * string

(* Variable declaration  *)
type var_decl =  typ * string * expr

(* Event formals *)
type event_formal = string

(* Function declaration *)
type func_decl = {
  typ : typ;
	fname : string;
	formals : bind list;
	locals : var_decl list;
	body : stmt list;
}

(* Events *)
type event = {
	evname : string;
  eformals : event_formal list;
  condition : expr;
  action : stmt list;
}

(* Elements *)
type element = {
	ename: string;
	e_properties: var_decl list;
}

(* World *)
type world = {
  w_properties: var_decl list;
  init_locals : var_decl list;
  init_body: stmt list;
}

(* Program *)
type program = var_decl list * func_decl list * event list * element list * world



(* AST Print functions *)

let string_of_typ = function
  Int -> "int"
| Float -> "float"
| String -> "string"
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

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | New(a,b,c) -> "element " ^ a ^ " = new " ^ b ^ string_of_expr c ^ ";\n"
  | Condition(st1, st2) -> string_of_stmt st1 ^ string_of_stmt st2
  | ECall(f,evnt) -> f ^ "(" ^ evnt ^ ")"

let string_of_vars = function
  (t,s,e) -> string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n"

let string_of_args (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_event_formals (id) = id

let string_of_fdecl fdecl =
  "\ndef " ^ string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ") {\n" ^
  String.concat "" (List.map string_of_vars fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_events event =
  "\nevent " ^ event.evname ^ "(" ^ String.concat ""
  (List.map string_of_event_formals event.eformals) ^ ") " ^
  "{\n " ^ "condition = " ^ (string_of_expr event.condition) ^ ";\n" ^
  "action {\n" ^ String.concat "" (List.map string_of_stmt event.action) ^
  "\n}\n"

let string_of_elems elem =
  "\nelement " ^ elem.ename ^ " " ^
  "{\n " ^
  String.concat " " (List.map string_of_vars elem.e_properties) ^
  "}\n"

let string_of_world world =
  "\nworld {\nproperties {\n" ^
  String.concat "" (List.map string_of_vars world.w_properties) ^ "}\n" ^
  String.concat "" (List.map string_of_vars world.init_locals) ^
  String.concat "" (List.map string_of_stmt world.init_body) ^
  "}\n"

let string_of_program (globals, funcs, events, elems, world) =
  String.concat " " (List.map string_of_vars globals) ^
  String.concat " " (List.map string_of_fdecl funcs) ^
  String.concat " " (List.map string_of_events events) ^
  String.concat " " (List.map string_of_elems elems) ^
  string_of_world world
