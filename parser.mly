/* Ocamlyacc parser for CRAFT */

%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE SEMI COLON COMMA
%token PLUS MINUS TIMES DIVIDE
%token ASSIGN EQ NEQ LT LEQ GT GEQ AND OR NOT
%token PERIOD COLLIDE
%token IF ELSE WHILE RETURN
%token INT FLOAT BOOL VOID TRUE FALSE
%token SIZE DIRECT COLOR PAIR SPEED POS NEW ACT COND
%token EVENT DEF PROPS ELEMENT WORLD START
%token KEY_PRS

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
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
  var_decl_list func_decl_list event_list element_list world EOF 
  { (List.rev $1, List.rev $2, List.rev $3, List.rev $4, $5) }

/* Primitive types */
typ:
    INT   { Int }
  | FLOAT { Float }
  | BOOL  { Bool }
  | VOID  { Void }
  | COLOR { Color }
  | PAIR  { Pair }

/* Variables*/
var_decl_list:             	  
  { [] }
  | var_decl_list var_decl 		{ $2 :: $1 }

var_decl:
  typ ID ASSIGN expr SEMI 	  { ($1, $2, $4) }

/* Element declaration */
element_decl:
  ELEMENT ID ID ASSIGN NEW ID expr SEMI  { New($2,$3,$6,$7) }

/* Functions */
func_decl_list:                   
  { [] }
  | func_decl_list func_decl 	{ $2 :: $1 }

func_decl:
  DEF typ ID LPAREN formals_list_opt RPAREN LBRACE var_decl_list stmt_list RBRACE
  {{
    typ = $2 ;
    fname = $3;
		formals = $5;
		locals = List.rev $8;
    body = List.rev $9;
  }}

/* Function arguments */
formals_list_opt:             
  { [] }
  | formals_list               { List.rev $1 }

formals_list:
    typ ID                     { [($1,$2)] }
  | formals_list COMMA typ ID  { ($3,$4) :: $1 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

/* Properties */
property_list:            
  { [] }
  | property_list property 	       { $2 :: $1 }

property:
    var_decl                 { $1 }
	| SIZE ASSIGN expr SEMI 	 { (Pair, "size", $3) }
	| COLOR ASSIGN expr SEMI 	 { (Color, "color", $3) }

/* Event arguments */
event_formals_list:
    ID ID                           { [($1,$2)] }
  | event_formals_list COMMA ID ID  { ($3,$4) :: $1 }

/* Events */
event_list:
  { [] }
  | event_list event  { $2 :: $1 }

event:
  EVENT ID LPAREN event_formals_list RPAREN LBRACE COND ASSIGN expr SEMI ACT LBRACE stmt_list RBRACE RBRACE
  {{
    evname = $2;
    eformals = $4;
    condition = $9;
    action = List.rev $13;
  }}

/* Elements */
element_list: 
  { [] } 
  | element_list element 	{ $2 :: $1 }

element: 
	ELEMENT ID LBRACE property_list RBRACE
  {{ 
    ename = $2;
    e_properties = List.rev $4; 
  }}

/* World */
world:
	WORLD LBRACE PROPS LBRACE property_list RBRACE var_decl_list stmt_list RBRACE   
	{{
    w_properties = List.rev $5;
    init_locals = List.rev $7;
    init_body = List.rev $8;
	}}

/* Statements */
stmt_list:          { [] }
  | stmt_list stmt	{ $2 :: $1 }

stmt:
	  expr SEMI 									              { Expr $1 }
  | element_decl                              { $1 }                          
	| RETURN expr SEMI 							            { Return $2 }
	| LBRACE stmt_list RBRACE 					        { Block(List.rev $2) }
	| IF LPAREN expr RPAREN stmt  %prec NOELSE 	{ If($3, $5, Block([])) }
	| IF LPAREN expr RPAREN stmt ELSE stmt 		  { If($3, $5, $7) }
	| WHILE LPAREN expr RPAREN stmt 			      { While($3, $5)}

/* Expressions */
expr:
    literals                                      { $1 }
	| expr PLUS expr                                { Binop($1, Add, $3) }
  | expr MINUS expr                               { Binop($1, Sub, $3) }
  | expr TIMES expr 				                      { Binop($1, Mult, $3) }
  | expr DIVIDE expr 				                      { Binop($1, Div, $3) }
  | expr EQ expr 					                        { Binop($1, Equal, $3) }
  | expr NEQ expr 				                        { Binop($1, Neq, $3) }
  | expr LT expr  			 	                        { Binop($1, Less, $3) }
  | expr LEQ expr  				                        { Binop($1, Leq, $3) }
  | expr GT expr 					                        { Binop($1, Greater, $3) }
  | expr GEQ expr 				                        { Binop($1, Geq, $3) }
  | expr AND expr 				                        { Binop($1, And, $3) }
  | expr OR expr 					                        { Binop($1, Or, $3) }
  | NOT expr  					                          { Unop(Not, $2) }
  | MINUS expr %prec NEG 		                      { Unop(Neg, $2) }
  | expr ASSIGN expr                              { Assign($1, $3) }
  | ID PERIOD POS PERIOD expr                     { PosAccess($1, $5) }
  | ID LPAREN actuals_opt RPAREN                  { Call($1, $3) }
  | ID LPAREN ID LPAREN actuals_opt RPAREN RPAREN { ECall($1, $3, $5) }
  | LPAREN expr RPAREN 			                      { $2 }
  | LPAREN expr COMMA expr RPAREN                 { Pr($2,$4) }
  | KEY_PRS LPAREN expr RPAREN                    { Keypress($3) }

literals:
	  INT_LITERAL 					        { ILiteral($1) }
	| FLOAT_LITERAL 				        { FLiteral($1) }
  | STRING_LITERAL                { SLiteral($1) }
	| TRUE							            { BLiteral(true) }
	| FALSE							            { BLiteral(false) }
  | ID  			                    { Id($1) }
  | COLOR                         { Id("color") }
  | SIZE                          { Id("size") }
