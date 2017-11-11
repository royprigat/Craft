(* CRAFT Scanner/Lexer *)

{ 
  open Parser 
}

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }  (* Whitespace *)
  | '#'         { comment lexbuf }       (* Comments *)
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LBRACE }
  | '}'         { RBRACE }
  | ';'         { SEMI }
  | ','         { COMMA }
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '*'         { TIMES }
  | '/'         { DIVIDE }
  | '='         { ASSIGN }
  | "=="        { EQ }
  | "!="        { NEQ }
  | '<'         { LT }
  | "<="        { LEQ }
  | ">"         { GT }
  | ">="        { GEQ }
  | "&&"        { AND }
  | "||"        { OR }
  | "!"         { NOT }
  | "."	        { PERIOD }
  | "!!"	      { COLLIDE }
  | "if"        { IF }
  | "else"      { ELSE }
  | "while"     { WHILE }
  | "int"       { INT }
  | "float"     { FLOAT }
  | "bool"      { BOOL }
  | "void"      { VOID }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "element"   { ELEMENT }
  | "elements"  { ELEMENTS }
  | "world"     { WORLD}
  | "event"     { EVENT }
  | "start"     { START }
  | "reset"     { RESET }
  | "def"       { DEF }
  | "return"    { RETURN }
  | "condition" { COND }
  | "health"    { HEALTH }
  | "lives"     { LIVES }
  | "action"    { ACTION }
  | "color"     { COLOR }
  | "new"       { NEW }
  | "delete"    { DELETE }
  | "speed"	    { SPEED }
  | "angle"	    { ANGLE }
  | "direction"	{ DIRECT }
  | "pos"	      { POS }
  | "this"	    { THIS }
  | "bounce"    { BOUNCE }
  | "import"    { IMPORT }
  | "key_up"    { KEY_UP }
  | "key_down"  { KEY_DOWN }
  | "key_id"    { KEY_ID }
  | ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
  | ['0'-'9']*'.'['0'-'9']+ | ['0'-'9']+'.'['0'-'9']* as lxm { FLOAT_LITERAL(float_of_string lxm)}
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

  and comment = parse
    '\n' { token lexbuf }
    | _    { comment lexbuf }
