(* CRAFT Scanner *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }    (* Whitespace *)
  | '#'                { comment lexbuf }  (* Comments *)
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LBRACE }
  | '}'         { RBRACE }
  | ';'         { SEMI }
  | ':'         { COLON }
  | ','         { COMMA }

  (* Operators *)
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '*'         { TIMES }
  | '/'         { DIVIDE }

  (* More Operators *)
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

  (* Special Operators *)
  | "."	        { PERIOD }
  | "!!"	      { COLLIDE }

  (* Control Flow *)
  | "if"        { IF }
  | "else"      { ELSE }
  | "while"     { WHILE }

  (* Keywords *)
  | "int"       { INT }
  | "float"     { FLOAT }
  | "bool"      { BOOL }
  | "void"      { VOID }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "size"      { SIZE }
  | "color"     { COLOR }
  | "pos"	      { POS }
  | "new"       { NEW }
  | "action"    { ACT }
  | "condition" { COND }
  | "return"    { RETURN }

  | "properties" { PROPS }
  | "event"      { EVENT }
  | "element"    { ELEMENT }
  | "world"      { WORLD}
  | "def"        { DEF }

  (* I/O Keywords *)
  | "key_press" { KEY_PRS }


  | ['0'-'9']+ as lxm                                        { INT_LITERAL(int_of_string lxm) }
  | ['0'-'9']*'.'['0'-'9']+ | ['0'-'9']+'.'['0'-'9']* as lxm { FLOAT_LITERAL(float_of_string lxm)}
  | '"' ((['a'-'z' 'A'-'Z' '0'-'9'])* as lxm) '"'            { STRING_LITERAL(lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm   { ID(lxm) }
  | eof                                                      { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


  and comment = parse
    '\n' { token lexbuf }
    | _    { comment lexbuf }
