%{
#include <stdio.h>     /* C declarations used in actions */
#include <stdlib.h>
%}

// Identifiers & numbers 
%token IDENTIFIER
%token NUMBER

// Keywords
%token IMPORT
%token CLASS
%token EXTENDS
%token PRIVATE
%token PUBLIC
%token STATIC
%token VOID
%token IF
%token ELSE
%token WHILE
%token LOOP
%token RETURN
%token PRINT
%token _NULL
%token NEW
%token INT
%token REAL
// Delimiters


%token LBRACE      //  {
%token RBRACE      //  }
%token LPAREN      //  (
%token RPAREN      //  )
%token LBRACKET    //  [
%token RBRACKET    //  ]
%token COMMA       //  ,
%token DOT         //  .
%token SEMICOLON   //  ;

// Operator signs
%token ASSIGN      //  =
%token LESS        //  <
%token GREATER     //  >
%token EQUAL       //  ==
%token NOT_EQUAL   //  !=
%token PLUS        //  +
%token MINUS       //  -
%token MULTIPLY    //  *
%token DIVIDE      //  /

%start CompilationUnit

%%
CompilationUnit
       : Imports ClassDeclarations	{;}
       ;

Imports
       :  /* empty */
       | Import Imports			{;}
       ;

Import
       : IMPORT IDENTIFIER SEMICOLON	{;}
       ;

ClassDeclarations
       : /* empty */
       | ClassDeclaration ClassDeclarations	{;}
       ;

ClassDeclaration
       :        CLASS CompoundName Extension SEMICOLON ClassBody	{;}
       | PUBLIC CLASS CompoundName Extension SEMICOLON ClassBody	{;}
       ;

Extension
       : /* empty */			{;}
       | EXTENDS IDENTIFIER		{;}
       ;

ClassBody
       : LBRACE              RBRACE	{;}
       | LBRACE ClassMembers RBRACE	{;}
       ;

ClassMembers
       :              ClassMember	{;}
       | ClassMembers ClassMember	{;}
       ;

ClassMember
       : FieldDeclaration		{;}
       | MethodDeclaration		{;}
       ;

FieldDeclaration
       : Visibility Staticness Type IDENTIFIER SEMICOLON		{;}
       ;

Visibility
       : /* empty */			{;}
       | PRIVATE			{;}
       | PUBLIC				{;}
       ;

Staticness
       : /* empty */
       | STATIC				{;}
       ;

MethodDeclaration
       : Visibility Staticness MethodType IDENTIFIER Parameters		
            Body			{;}
       ;

Parameters
       : LPAREN               RPAREN	{;}
       | LPAREN ParameterList RPAREN	{;}
       ;

ParameterList
       :                     Parameter	{;}
       | ParameterList COMMA Parameter	{;}
       ;

Parameter
       : Type IDENTIFIER		{;}
       ;

MethodType
       : Type				{;}
       | VOID				{;}
       ;

Body
       : LBRACE LocalDeclarations Statements RBRACE	{;}
       ;

LocalDeclarations
       :                   LocalDeclaration		{;}
       | LocalDeclarations LocalDeclaration		{;}
       ;

LocalDeclaration
       : Type IDENTIFIER SEMICOLON			{;}
       ;

Statements
       :            Statement		{;}
       | Statements Statement		{;}
       ;

Statement
       : Assignment			{;}
       | IfStatement			{;}
       | WhileStatement			{;}
       | ReturnStatement		{;}
       | CallStatement			{;}
       | PrintStatement			{;}
       | Block				{;}
       ;

Assignment
       : LeftPart ASSIGN Expression SEMICOLON		{;}
       ;

LeftPart
       : CompoundName			{;}
       | CompoundName LBRACKET Expression RBRACKET	{;}
       ;

CompoundName
       :                  IDENTIFIER			{;}
       | CompoundName DOT IDENTIFIER			{;}
       ;

IfStatement
       : IF LPAREN Relation RPAREN Statement		{;}
       | IF LPAREN Relation RPAREN Statement ELSE Statement	{;}
       ;

WhileStatement
       : WHILE Relation LOOP Statement SEMICOLON	{;}
       ;

ReturnStatement
       : RETURN            SEMICOLON	{;}
       | RETURN Expression SEMICOLON	{;}
       ;

CallStatement
       : CompoundName LPAREN              RPAREN SEMICOLON	{;}
       | CompoundName LPAREN ArgumentList RPAREN SEMICOLON	{;}
       ;

ArgumentList
       :                    Expression	{;}
       | ArgumentList COMMA Expression	{;}
       ;

PrintStatement
       : PRINT Expression SEMICOLON	{;}
       ;

Block
       : LBRACE            RBRACE	{;}
       | LBRACE Statements RBRACE	{;}
       ;

Relation
       : Expression			{;}
       | Expression RelationalOperator Expression	{;}
       ;

RelationalOperator
       : LESS				{;}
       | GREATER			{;}
       | EQUAL				{;}
       | NOT_EQUAL			{;}
       ;

Expression
       :         Term Terms		{;}
       | AddSign Term Terms		{;}
       ;

AddSign
       : PLUS				{;}
       | MINUS				{;}
       ;

Terms
       : /* empty */			{;}
       | AddSign Term Terms		{;}
       ;

Term
       : Factor Factors			{;}
       ;

Factors
       : /* empty */			{;}
       | MultSign Factor Factors	{;}
       ;

MultSign
       : MULTIPLY			{;}
       | DIVIDE				{;}
       ;

Factor
       : NUMBER				{;}
       | LeftPart			{;}
       | _NULL				{;}
       | NEW NewType			{;}
       | NEW NewType LBRACKET Expression RBRACKET	{;}
       ;

NewType
       : INT				{;}
       | REAL				{;}
       | IDENTIFIER			{;}
       ;

Type
       : INT        ArrayTail		{;}
       | REAL       ArrayTail		{;}
       | IDENTIFIER ArrayTail		{;}
       ;

ArrayTail
       : /* empty */
       | LBRACKET RBRACKET		{;}
       ;

%%
void yyerror (char *s) {fprintf (stderr, "%s\n", s);} 

int main(void) {
	return yyparse();
}
