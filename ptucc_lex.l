%{
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "ptucc_parser.tab.h"
#include "cgen.h"

int line_num = 1;

/* 
	A primitive macro facility:
	just one macro is allowed to be defined! 
*/
#define MAXMACRO 32
char* mactable[MAXMACRO][2];
int mactable_size = 0;

/* Return 1 on success, 0 on failure (macro table full) */
int set_macro(char* name, char* def);

/* Return def for macro, or NULL if no such macro is defined. */
char* get_macro(char* name);

%}

ID [a-zA-Z_][0-9a-zA-Z_]*
DIGIT  [0-9]
NUMBER  {DIGIT}{DIGIT}*
INTEGER [0]|([1-9]{DIGIT}*)
DECIMAL ("."{NUMBER})|({INTEGER}("."{NUMBER}?)?)
REAL    [+-]?{DECIMAL}([eE][+-]?{INTEGER})?
/*STRING  '[^'\n]*'*/
STRING  \'[^'\\\n]*\'
CONST_STRING (\"(\\.|[^"\\\n])*\")|(\'(\\.|[^'\\\n])*\')
CASTING [(]{ID}[)]

%x comment
%x inline_comment
%x macro

%%

@defmacro[ \r\t]+       BEGIN(macro);
<macro>{ID}             {
							/* Store macro name */
							char* name = strdup(yytext);
							char* def = NULL;
							size_t deflen = 0;
							char c;

							FILE* deff = open_memstream(&def, &deflen);

							while((c=input())!='\n')
							   fputc(c, deff);
							fclose(deff);

							if(!set_macro(name, def))
								yyerror("lexer error: failed to define macro '%s'\n", name);
							BEGIN(INITIAL);
						};

program					return KW_PROGRAM;
begin 					return KW_BEGIN;
end						return KW_END;
and						return KW_AND;
array					return KW_ARRAY;
boolean					return KW_BOOLEAN;
char					return KW_CHAR;
div						return KW_DIV;
do						return KW_DO;
else					return KW_ELSE;
for						return KW_FOR;
function				return KW_FUNCTION;
goto					return KW_GOTO;
if						return KW_IF;
integer					return KW_INTEGER;
var						return KW_VAR;
mod						return KW_MOD;
not						return KW_NOT;
of						return KW_OF;
or						return KW_OR;
while					return KW_WHILE;
procedure				return KW_PROCEDURE;
real					return KW_REAL;
repeat					return KW_REPEAT;
to						return KW_TO;
result					return KW_RESULT;
return					return KW_RETURN;
then					return KW_THEN;
until					return KW_UNTIL;
downto					return KW_DOWNTO;
true					return KW_TRUE;
false					return KW_FALSE;


{CONST_STRING}			{ yylval.crepr = strdup(yytext); return CONST_STRING; }
{STRING}				{ yylval.crepr = strdup(yytext); return STRING; }

{ID}  					{
						   char* def = get_macro(yytext);
						   if(def==NULL) {
						   		yylval.crepr = strdup(yytext);
						   		return IDENT;
						   	}
						   for(int i=strlen(def); i>0; i--) {
						   	  unput(def[i-1]);
						   }
						}
{INTEGER}				{ yylval.crepr = strdup(yytext); return INT; }
{NUMBER}				{ yylval.crepr = strdup(yytext); return POSINT; }
{REAL}					{ yylval.crepr = strdup(yytext); return REAL; }
{CASTING}				{ yylval.crepr = strdup(yytext); return CASTING; }

"+"						return '+';
"-"						return '-';
"*"						return '*';
"/"						return '/';
"="						return '=';
"<>"					return OP_DIFFERENT;
"<"						return '<';
"<="					return OP_LESS_EQUAL;
">"						return '>';
">="					return OP_GREATER_EQUAL;
"&&"					return OP_AND;
"||"					return OP_OR;
"!"						return '!';
":="					return OP_ASSIGN;
";"						return ';';
"("						return '(';
")"						return ')';
","						return ',';
"["						return '[';
"]"						return ']';
":"						return ':';
"."						return '.';

"//"						BEGIN(inline_comment);
<inline_comment>[^\n]*      /* eat anything that's not a \n */
<inline_comment>\n          {++line_num; BEGIN(INITIAL);}

"(*"						BEGIN(comment);
<comment>[^*\n]*        	/* eat anything that's not a '*' */
<comment>"*"+[^*)\n]*   	/* eat up '*'s not followed by ')'s */
<comment>\n             	++line_num;
<comment>"*"+")"        	BEGIN(INITIAL);


[ \r\t]					/* skip whitespace */
\n 						++line_num;
<<EOF>> 				return EOF;

.        				{ yyerror("lexical error: unrecognized literal '%s'\n", yytext); }

%%

int set_macro(char* name, char* def)
{
	/* Check to see if macro already defined, and redefine it. */
	int i;
	for(i=0; i<mactable_size; i++) {
		if(strcmp(mactable[i][0], name)==0) {
			/* found ! */
			free(name);
			free(mactable[i][1]);
			mactable[i][1] = def;
			break;
		}
	}
	if(i<mactable_size)
		return 1;
	else if(mactable_size < MAXMACRO) {
		/* new entry */
		assert(i==mactable_size);
		mactable[i][0] = name;
		mactable[i][1] = def;
		mactable_size++;
		return 1;
	}
	else
		return 0;
}

char* get_macro(char* name)
{
	for(int i=0;i<mactable_size; i++) {
		if(strcmp(mactable[i][0], name)==0)
			return mactable[i][1];
	}
	return NULL;
}


