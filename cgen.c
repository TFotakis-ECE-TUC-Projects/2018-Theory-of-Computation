#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include "cgen.h"

extern int line_num;

void ssopen(sstream* S){
	S->stream = open_memstream(& S->buffer, & S->bufsize);
}

char* ssvalue(sstream* S){
	fflush(S->stream);
	return S->buffer;
}

void ssclose(sstream* S){
	fclose(S->stream);
}


char* template(const char* pat, ...){
	sstream S;
	ssopen(&S);

	va_list arg;
	va_start(arg, pat);
	vfprintf(S.stream, pat, arg );
	va_end(arg);

	char* ret = ssvalue(&S);
	ssclose(&S);
	return ret;
}

/* Helper functions */
char* string_ptuc2c(char* P){
	int Plen = strlen(P);
	assert(Plen>=2);
	P[0] = '"';
	P[Plen-1] = '"';
	char *result = "\"";
	char *token1 = strtok(P, "\"");
	char *token2 = strtok(NULL, "\"");
	while(token1 != NULL){
		if(token2 == NULL)
			result = template("%s%s", result, token1);
		else
			result = template("%s%s\\\"", result, token1);
		token1 = token2;
		token2 = strtok(NULL, "\"");
	}
	result = template("%s\"", result);
	return result;
}


/*
	Report errors 
*/
 void yyerror (char const *pat, ...) {
 	va_list arg;
    fprintf (stderr, "Syntax error in line %d: ", line_num);
    va_start(arg, pat);
    vfprintf(stderr, pat, arg);
    va_end(arg);
 }

int yyerror_count = 0;

const char* c_prologue = 
"#include \"ptuclib.h\"\n"
"\n"
;

char* getArrayDeclarationString(char* identifiers, char* dimensions){
	char *result;
	char *token = strtok(identifiers, ",");
	if(token!= NULL){
		result = template("%s%s", token, dimensions);
		token = strtok(NULL, ",");
	}
    while (token != NULL){
		result = template("%s, %s%s", result, token, dimensions);
        token = strtok(NULL, ",");
    }
	return result;
}

char* getArrayPointerDeclarationString(char* identifiers, char* type){
	char *result;
	char *token = strtok(identifiers, ",");
	if(token!= NULL){
		result = template("%s* %s", type, token);
		token = strtok(NULL, ",");
	}
    while (token != NULL){
		result = template("%s, %s* %s", result, type, token);
        token = strtok(NULL, ",");
    }
	return result;
}

char* getArrayDeclarationStringWithType(char* identifiers, char* dimensions, char* type){
	char *result;
	char *token = strtok(identifiers, ",");
	if(token!= NULL){
		result = template("%s %s%s", type, token, dimensions);
		token = strtok(NULL, ",");
	}
    while (token != NULL){
		result = template("%s, %s %s%s", result, type, token, dimensions);
        token = strtok(NULL, ",");
    }
	return result;
}

char* getParameterDeclarationString(char* identifiers, char* type){
	char *result;
	char *token = strtok(identifiers, ",");
	if(token!= NULL){
		result = template("%s %s", type, token);
		token = strtok(NULL, ",");
	}
    while (token != NULL){
		result = template("%s, %s %s", result, type, token);
        token = strtok(NULL, ",");
    }
	return result;
}

char* getFunctionPointerDeclaration(char* identifiers, char* type, char* parameters){
	char *result;
	char *token = strtok(identifiers, ",");
	if(token!= NULL){
		result = template("%s (*%s)(%s)", type, token, parameters);
		token = strtok(NULL, ",");
	}
    while (token != NULL){
		result = template("%s, (*%s)(%s)", result, token, parameters);
        token = strtok(NULL, ",");
    }
	result = template("%s;", result);
	return result;	
}

char* getFunctionPointerDeclarationAsParameters(char* identifiers, char* type, char* parameters){
	char *result;
	char *token = strtok(identifiers, ",");
	if(token!= NULL){
		result = template("%s (*%s)(%s)", type, token, parameters);
		token = strtok(NULL, ",");
	}
    while (token != NULL){
		result = template("%s, %s (*%s)(%s)", result, type, token, parameters);
        token = strtok(NULL, ",");
    }
	return result;
}