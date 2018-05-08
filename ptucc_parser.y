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
%type <crepr> subprogramBody
%type <crepr> subprogramParametersDeclaration
%type <crepr> subprogramParametersDeclarationList
%type <crepr> variableDeclarationOptional
%type <crepr> typedefsOptional
%type <crepr> typedefs
%type <crepr> typedefList
%type <crepr> typedefStatement
%type <crepr> command
%type <crepr> ifStatement
%type <crepr> flowControlBody
%type <crepr> forStatement
%type <crepr> whileStatement

%left KW_OR OP_OR
%left KW_AND OP_AND
%left '=' OP_DIFFERENT '<' '>' OP_LESS_EQUAL OP_GREATER_EQUAL
%left '+' '-'
%left '*' '/' KW_DIV KW_MOD
%nonassoc USIGN
%nonassoc UNOT

%right KW_THEN KW_ELSE

%%
empty: 
	{ $$ = ""; };

program:
	program_decl typedefsOptional variableDeclarationOptional subprogramList body  '.'
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

program_decl: 
	KW_PROGRAM IDENT ';' { $$ = $2; };

body: 
	KW_BEGIN statements KW_END { $$ = template("{\n%s\treturn 0;\n}\n", $2); };

statements: 
	empty
	| statement_list
	;

statement_list: 
	statement
	| statement_list ';' statement  { $$ = template("%s%s", $1, $3); }
	; 

statement: 
	proc_call  	{ $$ = template("\t%s;\n", $1); }
	| command 	{ $$ = template("\t%s;\n", $1); }
	;

proc_call: 
	IDENT '(' arguments ')' { $$ = template("%s(%s)", $1, $3); };

arguments: 
	empty | arglist { $$ = $1; };

arglist: 
	expression						{ $$ = $1; }
    | arglist ',' expression 		{ $$ = template("%s,%s", $1, $3);  }
	;

expression: 
	POSINT
	| INT
	| REAL
	| STRING 									{ $$ = string_ptuc2c($1); }
	| IDENT
	| '(' expression ')' 						{ $$ = template("(%s)", $2); }
	| '(' dataType ')' expression				{ $$ = template("(%s) %s", $2, $4); }
	| KW_NOT expression %prec UNOT				{ $$ = template("!%s", $2); }
	| "!" expression %prec UNOT					{ $$ = template("!%s", $2); }
	| '+' expression %prec USIGN				{ $$ = template("+%s", $2); }
	| '-' expression %prec USIGN				{ $$ = template("-%s", $2); }
	| expression '*' expression 				{ $$ = template("%s * %s", $1, $3); }
	| expression '/' expression 				{ $$ = template("%s / %s", $1, $3); }
	| expression KW_DIV expression 				{ $$ = template("%s / %s", $1, $3); }
	| expression KW_MOD expression 				{ $$ = template("%s % %s", $1, $3); }
	| expression '+' expression 				{ $$ = template("%s + %s", $1, $3); }
	| expression '-' expression 				{ $$ = template("%s - %s", $1, $3); }
	| expression '=' expression 				{ $$ = template("%s == %s", $1, $3); }
	| expression OP_DIFFERENT expression 		{ $$ = template("%s != %s", $1, $3); }
	| expression '<' expression 				{ $$ = template("%s < %s", $1, $3); }
	| expression '>' expression 				{ $$ = template("%s > %s", $1, $3); }
	| expression OP_LESS_EQUAL expression 		{ $$ = template("%s <= %s", $1, $3); }
	| expression OP_GREATER_EQUAL expression 	{ $$ = template("%s >= %s", $1, $3); }
	| expression KW_AND expression 				{ $$ = template("%s && %s", $1, $3); }
	| expression OP_AND expression 				{ $$ = template("%s && %s", $1, $3); }
	| expression KW_OR expression 				{ $$ = template("%s || %s", $1, $3); }
	| expression OP_OR expression 				{ $$ = template("%s || %s", $1, $3); }
	| proc_call
	;

variableDeclarationOptional: 
	empty | variableDeclaration;

variableDeclaration: 
	KW_VAR variableDeclarationList	{ $$ = template("%s", $2); };

variableDeclarationList: 
	variableDeclarationStatement 
	| variableDeclarationList variableDeclarationStatement
		{ $$ = template("%s%s", $1, $2); }
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
	| KW_ARRAY KW_OF basicDataType							
		{ $$ = template("%s*", $3); }
	| KW_ARRAY arrayDimensionDeclarator KW_OF basicDataType	
		{ $$ = template("%s%s", $4, $2); }
	| functionPointerType
	| IDENT
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
	KW_PROCEDURE IDENT '(' subprogramParametersDeclarationList ')' ';' typedefsOptional variableDeclarationOptional subprogramList subprogramBody	
		{ $$ = template("void %s(%s){\n%s%s%s%s\n}\n", $2, $4, $7, $8, $9, $10); }
	| KW_FUNCTION IDENT '(' subprogramParametersDeclarationList ')' ':' dataType ';' typedefsOptional variableDeclarationOptional subprogramList subprogramBody	
		{ $$ = template("%s %s(%s){\n%s\n%s result;\n%s%s%s\n}\n", $7, $2, $4, $9, $7, $10, $11, $12); }
	;

subprogramBody:	
	KW_BEGIN statements KW_END ';'	
		{ $$ = template("%s", $2); }
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


typedefsOptional: 
	empty | typedefs;

typedefs: 
	KW_TYPE typedefList { $$ = template("%s", $2); };

typedefList:
	typedefStatement
	| typedefList typedefStatement	{ $$ = template("%s%s", $1, $2); }
	;

typedefStatement: 
	IDENT '=' dataType ';'	{ $$ = template("typedef %s %s;\n", $3, $1); };

command:
	IDENT OP_ASSIGN expression 
		{ $$ = template("%s = %s;\n", $1, $3); }
	| KW_RESULT OP_ASSIGN expression 
		{ $$ = template("result = %s;\n", $3); }
	| KW_RETURN 
		{ $$ = template("return result;\n"); }
	| IDENT ':' command 
		{ $$ = template("%s: %s", $1, $3); }
	| KW_GOTO IDENT 
		{ $$ = template("goto %s;\n", $2); }
	| ifStatement
	| forStatement
	| whileStatement
	;

ifStatement:
	KW_IF expression KW_THEN flowControlBody
		{ $$ = template("if(%s){\n%s\n}\n", $2, $4); }
	| KW_IF expression KW_THEN flowControlBody KW_ELSE flowControlBody
		{ $$ = template("if(%s){\n%s\n}else{\n%s\n}\n", $2, $4, $6); }
	;

flowControlBody:
	statement
	| KW_BEGIN statements KW_END ';' { $$ = template("%s", $2); }
	;

forStatement:
	KW_FOR IDENT OP_ASSIGN expression KW_TO expression KW_DO flowControlBody 
		{ $$ = template("for(int %s=$s; i<=%s; i++){\n%s\n}\n", $2, $4, $6, $8); }
	| KW_FOR IDENT OP_ASSIGN expression KW_DOWNTO expression KW_DO flowControlBody 
		{ $$ = template("for(int %s=$s; i>=%s; i--){\n%s\n}\n", $2, $4, $6, $8); }
	;

whileStatement:
	KW_WHILE expression KW_DO flowControlBody 
		{ $$ = template("while(%s){\n%s\n}\n", $2, $4); }
	| KW_REPEAT flowControlBody KW_UNTIL expression 
		{ $$ = template("do{\n%s\n}while(%s);\n", $2, $4); }
	;
%%