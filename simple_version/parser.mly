

%{ open Ast %}

/* Ocamlyacc parser for CRAFT */

%token LPAREN RPAREN LBRACE RBRACE SEMI COLON COMMA
%token PLUS MINUS TIMES DIVIDE 
%token ASSIGN EQ NEQ LT LEQ GT GEQ AND OR NOT
%token PERIOD COLLIDE
%token IF ELSE WHILE 
%token INT FLOAT BOOL VOID TRUE FALSE
%token SIZE DIRECT COLOR PAIR SPEED POS
%token EVENT DEF ELEMENT WORLD

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
/* %token <string> STRING_LITERAL */
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
  world EOF { $1 }

/* Variable types */

typ:
    INT   { Int }
  | FLOAT { Float }
  | BOOL  { Bool }
  | VOID  { Void }


/* Initialize variable */
var_init: 
  typ ID ASSIGN expr SEMI 	{ SetVar($1, $2, $4) }



/* World */
world:
	WORLD LBRACE stmt_list RBRACE 	/* ELEMENT WAS ELEMS but that is not defined anywhere!!? */
	{{
    body = List.rev $3;
	}}

/* Statements */
stmt_list: 
	{ [] }
  | stmt_list stmt	{ $2 :: $1 }			

stmt:
  expr SEMI                                   { Expr $1 }
  | LBRACE stmt_list RBRACE                   { Block(List.rev $2) }
  /*| IF LPAREN expr RPAREN stmt  %prec NOELSE  { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt      { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt             { While($3, $5)} */

  
/* Expressions */
expr: 
	  INT_LITERAL 					  { ILiteral($1) }
	| FLOAT_LITERAL 				  { FLiteral($1) }
  | ID                      { Id($1) }
  /*| STRING_LITERAL          { SLiteral($1) }*/
	| TRUE							      { BLiteral(true) }
	| FALSE							      { BLiteral(false) }
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
  | NOT expr  					    { Unop(Not, $2) }
  /*| MINUS expr %prec NEG 		{ Unop(Neg, $2) } */
  | LPAREN expr RPAREN 			{ $2 }
  /*| ID LPAREN actuals_opt RPAREN        { Call($1,$3) }*/

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
