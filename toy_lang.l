%{
#include "y.tab.h"
%}
%%
"import"	{return IMPORT;}
"class" 	{return CLASS;}
"extends"	{return EXTENDS;}
"private"	{return PRIVATE;}
"public"	{return PUBLIC;}
"static"	{return STATIC;}
"void"		{return VOID;}
"if"		{return IF;}
"else" 		{return ELSE;}
"while"		{return WHILE;}
"loop"		{return LOOP;}
"return"	{return RETURN;}
"print"		{return PRINT;}
"null"		{return NULL;}
"new"		{return NEW;}
"int" 		{return INT;}
"real"		{return REAL;}
\{		{return LBRACE;}
\}		{return RBRACE;}
\(		{return LPAREN;}
\)		{return RPAREN;}
\[		{return LBRACKET;}
\]		{return RBRACKET;}
\,		{return COMMA;}
\.		{return DOT;}
\;		{return SEMICOLON;}
\={1}		{return ASSIGN;}
\<		{return LESS;}
\>		{return GREATER;}
\={2}		{return EQUAL;}
[\!\=]		{return NOT_EQUAL;}
\+		{return PLUS;}
\-		{return MINUS;}
\*		{return MULTIPLY;}
\/		{return DIVIDE;}
[a-zA-Z][_a-zA-Z0-9]*	{return IDENTIFIER;}
[0-9]+		{return NUMBER;}
[ \t\n]		;
.		{ECHO; yyerror("Unexpected Character");}
%%
int yywrap(void) {return 1;}
