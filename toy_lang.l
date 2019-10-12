%{
    extern "C"
    {
        int yylex(void);
        void yyerror (char *s);
    }

#include "y.tab.h"
#include <string>


%}


%%
"import"	{yylval.ID = "import"; return IMPORT;}
"class" 	{yylval.ID = "class"; return CLASS;}
"extends"	{yylval.ID = "extends"; return EXTENDS;}
"private"	{yylval.ID = "private"; return PRIVATE;}
"public"	{yylval.ID = "public"; return PUBLIC;}
"static"	{yylval.ID = "static"; return STATIC;}
"void"		{yylval.ID = "void"; return VOID;}
"if"		{yylval.ID = "if"; return IF;}
"else" 		{yylval.ID = "else"; return ELSE;}
"while"		{yylval.ID = "while"; return WHILE;}
"loop"		{yylval.ID = "loop"; return LOOP;}
"return"	{yylval.ID = "return"; return RETURN;}
"print"		{yylval.ID = "print"; return PRINT;}
"null"		{yylval.ID = "null"; return _NULL;}
"new"		{yylval.ID = "new"; return NEW;}
"int" 		{yylval.ID = "int"; return INT;}
"real"		{yylval.ID = "real"; return REAL;}
\{		{yylval.ID = "{"; return LBRACE;}
\(		{yylval.ID = "("; return LPAREN;}
\}		{yylval.ID = "}"; return RBRACE;}
\)		{yylval.ID = ")"; return RPAREN;}
\[		{yylval.ID = "["; return LBRACKET;}
\]		{yylval.ID = "]"; return RBRACKET;}
\,		{yylval.ID = ","; return COMMA;}
\.		{yylval.ID = "."; return DOT;}
\;		{yylval.ID = ";"; return SEMICOLON;}
\={1}		{yylval.ID = "="; return ASSIGN;}
\<		{yylval.ID = "<"; return LESS;}
\>		{yylval.ID = ">"; return GREATER;}
\={2}		{yylval.ID = "=="; return EQUAL;}
[\!\=]		{yylval.ID = "\!\="; return NOT_EQUAL;}
\+		{yylval.ID = "+"; return PLUS;}
\-		{yylval.ID = "-"; return MINUS;}
\*		{yylval.ID = "*"; return MULTIPLY;}
\/		{yylval.ID = "/"; return DIVIDE;}
\Z      {yylval.ID = "\Z"; return EOFF;}
[a-zA-Z_][a-zA-Z_0-9]*	{yylval.ID = yytext; return IDENTIFIER;}
[0-9]+\.?[0-9]*		{yylval.ID = yytext; return NUMBER;}
[ \t\n]		;
.		{yyerror("Unexpected Character");}
%%
int yywrap(void) {return 1;}
