/* Ocamlyacc parser for CRAFT */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE WHILE POINT COLLIDE
%token INT FLOAT BOOL VOID
%token SIZE DIR COLOR SPEED
%token EVENT FUNC ELEM ELEMS WORLD

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left COLLIDE
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG
%left ACCESS

%start program
%type <Ast.program> program

%%

/* Entry point */
program:
  element_list world EOF { ($1, $2) }

/* Primitive type */
primitive:
    INT   { Int }
  | FLOAT { Float }
  | BOOL  { Bool }
  | VOID  { Void }

/* Variable declaration */
var_decl: 
    primitive ID ASSIGN expr SEMI 	{ VarDec($1, $2, $4) }

element_list: 
  { [] }
  | element_list element 	{ $2 :: $1 }

/* Elements */
element: 
		ELEM ID LBRACE prop_list RBRACE
    {{ 
      name = $2;
      properties = List.rev $4 
    }}

prop_list:
	  { [] }
    | prop_list property 	{ $2 :: $1 }

property:
		var_decl 								   { $1 }
		| SIZE ASSIGN expr SEMI 	 { VarInit(Size, "size", $3) }
		| DIR ASSIGN expr SEMI		 { VarInit(Direction, "dir", $3) } 
		| COLOR ASSIGN expr SEMI 	 { VarInit(Color, "color", $3) } 
    | SPEED ASSIGN expr SEMI   { VarInit(Speed, "speed", $3) } 

/* World */
world:
		WORLD LBRACE prop_list ELEMS LBRACE stmt_list RBRACE RBRACE 	
		{{
      properties = List.rev $3;
			elements = List.rev $6;
		}}

stmt_list: 
	    { [] }
      | stmt_list stmt	{ $2 :: $1 }			

/* Statements */
stmt:
	expr SEMI 									                { Expr $1 }
	| RETURN expr SEMI 							            { Return $2 }
	| LBRACE stmt_list RBRACE 					        { Block(List.rev $2) }
	| IF LPAREN expr RPAREN stmt  %prec NOELSE 	{ If($3, $5, Block([])) }
	| IF LPAREN expr RPAREN stmt ELSE stmt 		  { If($3, $5, $7) }
	| WHILE LPAREN expr RPAREN stmt 			      { While($3, $5)}
  
/* Expressions */
expr: 
	  INT_LITERAL 					  { Literal($1) }
	| FLOAT_LITERAL 				  { FLiteral($1) }
	| TRUE							      { BoolLit(true) }
	| FALSE							      { BoolLit(false) }
	| expr PLUS expr          { Binop($1, Add, $3) }
  | expr MINUS expr         { Binop($1, Sub, $3) }
  | expr TIMES expr 				{ Binop($1, Mult, $3) }
  | expr DIVIDE expr 				{ Binop($1, Div, $3) }
  | expr EQ expr 					  { Binop($1, Equal, $3) }
  | expr NEQ expr 				  { Binop($1, Neq, $3) }
  | expr LT expr  			 	  { Binop($1, Less, $3) }
  | expr LEQ expr  				  { Binop($1, Leq, $3) }
  | expr GT expr 					  { Binop($1, Greater, $3) }
  | expr GEQ expr 				  { Binop($1, Geq, $3) }
  | expr AND expr 				  { Binop($1, And, $3) }
  | expr OR expr 					  { Binop($1, Or, $3) }
  | expr COLLIDE expr 			{ Binop($1, Coll, $3) }
  | NOT expr  					    { Unop(Not, $2) }
  | MINUS expr %prec NEG 		{ Unop(Neg, $2) }
  | LPAREN expr RPAREN 			{ $2 }
  | member 						      { $1 }

member:
	ID  				        { $1 }
	| ID PERIOD member  { Accs($1, $3) }
