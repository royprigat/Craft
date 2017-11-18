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

  | "element"   { ELEMENT }
  | "world"     { WORLD}
  | "event"     { EVENT }
  | "def"       { DEF }
  | "pos"	      { POS }

  (* I/O Keywords *)
  (*
  | "key_up"    { KEY_UP }
  | "key_down"  { KEY_DOWN }
  | "key_id"    { KEY_ID }
  *)


  | ['0'-'9']+ as lxm                                        { INT_LITERAL(int_of_string lxm) }
  | ['0'-'9']*'.'['0'-'9']+ | ['0'-'9']+'.'['0'-'9']* as lxm { FLOAT_LITERAL(float_of_string lxm)}
  (*| '"' ((['a'-'z' 'A'-'Z'])* as lxm) '"'                    { STRING_LITERAL(lxm) } *)
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm   { ID(lxm) }
  | eof                                                      { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


  and comment = parse
    '\n' { token lexbuf }
    | _    { comment lexbuf }
