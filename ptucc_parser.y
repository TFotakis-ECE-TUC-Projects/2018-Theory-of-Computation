%{
#include <stdarg.h>
#include <stdio.h>	
#include "cgen.h"

extern int yylex(void);
extern int line_num;
%}

%union{
	char* crepr;
}


%token <crepr> IDENT
%token <crepr> POSINT
%token <crepr> INT 
%token <crepr> REAL 
%token <crepr> STRING

%token KW_PROGRAM 
%token KW_BEGIN
%token KW_END
%token KW_AND
%token KW_ARRAY
%token KW_BOOLEAN
%token KW_CHAR
%token KW_DIV
%token KW_DO
%token KW_ELSE
%token KW_FOR
%token KW_FUNCTION
%token KW_GOTO
%token KW_IF
%token KW_INTEGER
%token KW_VAR
%token KW_MOD
%token KW_NOT
%token KW_OF
%token KW_OR
%token KW_WHILE
%token KW_PROCEDURE
%token KW_REAL
%token KW_REPEAT
%token KW_TO
%token KW_RESULT
%token KW_RETURN
%token KW_THEN
%token KW_UNTIL
%token KW_DOWNTO
%token KW_TRUE
%token KW_FALSE
%token KW_TYPE
%token CONST_STRING
%token CASTING
%token OP_DIFFERENT
%token OP_LESS_EQUAL
%token OP_GREATER_EQUAL
%token OP_AND
%token OP_OR
%token OP_ASSIGN

%start program

%type <crepr> program_decl 
%type <crepr> body
%type <crepr> statements
%type <crepr> statement_list
%type <crepr> statement
%type <crepr> proc_call
%type <crepr> arguments
%type <crepr> arglist
%type <crepr> expression
%type <crepr> dataType
%type <crepr> variableDeclaration
%type <crepr> variableDeclarationStatement
%type <crepr> variableDeclarationList
%type <crepr> identifierList
%type <crepr> arrayDimensionDeclarator
%type <crepr> arrayIndexer
%type <crepr> parametersDeclaration
%type <crepr> basicDataType
%type <crepr> functionPointerDeclaration
%type <crepr> functionPointerType
%type <crepr> empty
%type <crepr> subprogram
%type <crepr> subprogramList
%type <crepr> subprogramTitle
%type <crepr> subprogramBody
%type <crepr> subprogramParametersDeclaration
%type <crepr> subprogramParametersDeclarationList
%type <crepr> variableDeclarationOptional
%type <crepr> typedefsOptional
%type <crepr> typedefs
%type <crepr> typedefList
%type <crepr> typedefStatement

%left '-' '+'
%left '*' '/'

%%

empty: { $$ = ""; };

program:  program_decl typedefsOptional variableDeclarationOptional subprogramList body  '.'
	{ 
		/* We have a successful parse! 
			Check for any errors and generate output. 
		*/
		if(yyerror_count==0) {
			puts(c_prologue);
			printf("/* program  %s */ \n\n", $1);
			printf("%s\n\n", $2);
			printf("%s\n\n", $3);
			printf("%s\n\n", $4);
			printf("int main() %s \n", $5);
		}
	}
	;

program_decl: KW_PROGRAM IDENT ';'  	{ $$ = $2; };

body: KW_BEGIN statements KW_END   		{ $$ = template("{\n %s \n }\n", $2); };

statements: 
	empty
	| statements statement_list			{ $$ = template("%s%s", $1, $2); }
	;

statement_list: 
	statement
	| statement_list ';' statement  { $$ = template("%s%s", $1, $3); }
	; 

statement: proc_call  				{ $$ = template("%s;\n", $1); };

proc_call: IDENT '(' arguments ')' 	{ $$ = template("%s(%s)", $1, $3); };

arguments: empty | arglist 			{ $$ = $1; };

arglist: 
	expression						{ $$ = $1; }
    | arglist ',' expression 		{ $$ = template("%s,%s", $1, $3);  }
	;

expression: 
	INT							
	| REAL							
	| STRING 						{ $$ = string_ptuc2c($1); }
	| '(' expression ')' 			{ $$ = template("(%s)", $2); }
	| expression '+' expression 	{ $$ = template("%s + %s", $1, $3); }
	| expression '-' expression 	{ $$ = template("%s - %s", $1, $3); }
	| expression '*' expression 	{ $$ = template("%s * %s", $1, $3); }
	| expression '/' expression 	{ $$ = template("%s / %s", $1, $3); }
	;

variableDeclaration: KW_VAR variableDeclarationList	{ $$ = template("%s", $2); };

variableDeclarationList: 
	variableDeclarationStatement 
	| variableDeclarationList variableDeclarationStatement { $$ = template("%s%s", $1, $2); }
	;

variableDeclarationStatement:
	identifierList ':' basicDataType ';'
		{ $$ = template("%s %s;\n", $3, $1); }
	| identifierList ':' KW_ARRAY KW_OF basicDataType ';'
		{ $$ = template("%s* %s;\n", $5, $1); }
	| identifierList ':' KW_ARRAY arrayDimensionDeclarator KW_OF basicDataType ';'
		{ $$ = template("%s %s;\n", $6, getArrayDeclarationString($1, $4)); }
	| functionPointerDeclaration ';'
		{ $$ = template("%s\n", $1); }
	;

identifierList: 
	IDENT
	| identifierList ',' IDENT	{ $$ = template("%s, %s", $1, $3); }
	;

dataType:
	basicDataType
	| KW_ARRAY KW_OF basicDataType							{ $$ = template("%s*", $3); }
	| KW_ARRAY arrayDimensionDeclarator KW_OF basicDataType	{ $$ = template("%s%s", $4, $2); }
	| functionPointerType
	;

basicDataType:
	KW_INTEGER 		{ $$ = "int"; }
	| KW_BOOLEAN	{ $$ = "int"; }
	| KW_CHAR		{ $$ = "char"; }
	| KW_REAL		{ $$ = "double"; }
	;

arrayDimensionDeclarator:
	'[' POSINT ']'					{ $$ = template("[%s]", $2); }
	| arrayIndexer '[' POSINT ']'	{ $$ = template("%s[%s]", $1, $3); }
	;

arrayIndexer:
	 '[' IDENT ']'					{ $$ = template("[%s]", $2); }
	| arrayIndexer '[' IDENT ']'	{ $$ = template("%s[%s]", $1, $3); }
	| '[' POSINT ']'				{ $$ = template("[%s]", $2); }
	| arrayIndexer '[' POSINT ']'	{ $$ = template("%s[%s]", $1, $3); }
	;

functionPointerDeclaration:
	identifierList ':' KW_FUNCTION '(' parametersDeclaration ')' ':' dataType	{ $$ = getFunctionPointerDeclaration($1, $8, $5); }
	;

functionPointerType:
	KW_FUNCTION '(' parametersDeclaration ')' ':' dataType { $$ = template("%s(*)(%s)", $6, $3); };

parametersDeclaration:
	identifierList ':' dataType									{ $$ = getParameterDeclarationString($1, $3); }
	| parametersDeclaration ',' identifierList ':' dataType 	{ $$ = template("%s, %s", $1, getParameterDeclarationString($3, $5)); }
	;

subprogramList:
	empty
	| subprogramList subprogram { $$ = template("%s%s", $1, $2); }
	;

subprogram:
	subprogramTitle typedefsOptional variableDeclarationOptional subprogramList subprogramBody	{ $$ = template("%s{\n%s%s%s%s\n}\n", $1, $2, $3, $4, $5); }
	;

subprogramBody:	KW_BEGIN statements KW_END ';'	{ $$ = template("%s", $2); };

subprogramTitle:
	KW_PROCEDURE IDENT '(' subprogramParametersDeclarationList ')' ';'	{ $$ = template("void %s(%s)", $2, $4); }
	| KW_FUNCTION IDENT '(' subprogramParametersDeclarationList ')' ':' dataType ';'	{ $$ = template("%s %s(%s)", $7, $2, $4); }
	;

subprogramParametersDeclarationList:
	empty
	| subprogramParametersDeclaration
	| subprogramParametersDeclarationList ';' subprogramParametersDeclaration { $$ = template("%s, %s", $1, $3); }
	;

subprogramParametersDeclaration:
	identifierList ':' basicDataType
		{ $$ = getParameterDeclarationString($1, $3); }
	| identifierList ':' KW_ARRAY KW_OF basicDataType
		{ $$ = getArrayPointerDeclarationString($1, $5); }
	| identifierList ':' KW_ARRAY arrayDimensionDeclarator KW_OF basicDataType
		{ $$ = getArrayDeclarationStringWithType($1, $4, $6); }
	| identifierList ':' KW_FUNCTION '(' parametersDeclaration ')' ':' dataType
		{ $$ = getFunctionPointerDeclarationAsParameters($1, $8, $5); }
	;

variableDeclarationOptional: empty | variableDeclaration;

typedefsOptional: empty | typedefs;

typedefs: KW_TYPE typedefList	{ $$ = template("%s", $2); };

typedefList:
	typedefStatement
	| typedefList typedefStatement	{ $$ = template("%s%s", $1, $2); }
	;

typedefStatement: IDENT '=' dataType ';'	{ $$ = template("typedef %s %s;\n", $3, $1); };

%%