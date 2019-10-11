%{
#include <stdio.h>     /* C declarations used in actions */
#include <stdlib.h>
#include <string>
#include <iostream>
#include <math.h>

using namespace std;

#define SPACES_ADD 4

extern "C"
{
       int yylex();
       int yyparse();

       void yyerror (char *s) {fprintf (stderr, "%s\n", s);} 
}

int relation(double first, double second, int sign);

int spaces = 0;

void print_token(string token) {
       for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
       cout << token << "\n";
}

struct ArrayTail {
       string t_lbracket, t_rbracket;
       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "ArrayTail\n";
              spaces += SPACES_ADD;
              if (t_lbracket) print_token(t_lbracket);
              if (t_rbracket) print_token(t_rbracket);
              spaces -= SPACES_ADD;
       }
};

struct Type {
       string token;
       struct ArrayTail *arraytail;
       
       Type() : arraytail(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "Type\n";
              spaces += SPACES_ADD;
              print_token(token);
              if (arraytail) arraytail->traverse();
              spaces -= SPACES_ADD;
       }
};

struct CompoundName {
       string t_dot, t_id;
       struct CompoundName *compoundname;
       CompoundName() : compoundname(nullptr) {};

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "CompoundName\n";
              spaces += SPACES_ADD;
              if (compoundname) compoundname->traverse();
              if (t_dot) print_token(t_dot);
              if (t_id) print_token(t_id);
              spaces -= SPACES_ADD;
       }
};

struct LeftPart {
       string t_lbracket, t_rbracket;
       struct Expression *expression;
       struct CompoundName *compoundname;
       LeftPart() : expression(nullptr), compoundname(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "LeftPart\n";
              spaces += SPACES_ADD;
              if (compoundname) compoundname->traverse();
              if (t_lbracket) print_token(t_lbracket);
              if (expression) expression->traverse()
              if (t_rbracket) print_token(t_rbracket);
              spaces -= SPACES_ADD;
       }
};

struct NewType {
       string token;
       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "NewType\n";
              spaces += SPACES_ADD;
              print_token(token);
              spaces -= SPACES_ADD;
       }
};

struct Factor {
    string t_number, t_new, t_null, t_lbracket, t_rbracket;
    struct Expression *expression;
    struct NewType *newtype;
    struct LeftPart *leftpart;

    Factor() : expression(nullptr), newtype(nullptr) {}

    void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "Factor\n";
              spaces += SPACES_ADD;
              
              if (t_number) print_token(t_number);
              if (t_null) print_token(t_null);
              if (leftpart) leftpart->traverse();
              if (t_new) print_token(t_new);
              if (newtype) newtype->traverse();
              if (t_lbracket) print_token(t_lbracket);
              if (expression) expression->traverse();
              if (t_rbracket) print_token(t_rbracket);

              spaces -= SPACES_ADD;
    }
};

struct MultSign {
    string multsign;
    void traverse() {
           for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
           cout << "MultSign\n";
           spaces += SPACES_ADD;
           if (multsign) print_token(multsign);
           spaces -= SPACES_ADD;
    }
};

struct Factors {
    struct MultSign *multsign;
    struct Factor *factor;
    struct Factors *factors;
    Factors() : multsign(nullptr), factor(nullptr), factors(nullptr) {}
    void traverse() {
           for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
           cout << "Factors\n";
           spaces += SPACES_ADD;
           if (multsign) multsign->traverse();
           if (factor) factor->traverse();
           if (factors) factors->traverse();
           spaces -= SPACES_ADD;
    }
};

struct Term {
    struct Factor *factor;
    struct Factors *factors;
    Term() : factor(nullptr), factors(nullptr) {}
    void traverse() {
           for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
           cout << "Term\n";
           spaces += SPACES_ADD;
           if (factor) factor->traverse();
           if (factors) factors->traverse();
           spaces -= SPACES_ADD;
    }
};

struct AddSign {
    string addsign;
    void traverse() {
           for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
           cout << "AddSign\n";
           spaces += SPACES_ADD;
           if (addsign) print_token(addsign);
           spaces -= SPACES_ADD;
    }
};

struct Terms {
    struct AddSign *addsign;
    struct Term *term;
    struct Terms *terms;
    Terms() : addsign(nullptr), term(nullptr), terms(nullptr) {}
    void traverse() {
           for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
           cout << "Terms\n";
           spaces += SPACES_ADD;
           if (addsign) addsign->traverse();
           if (term) term->traverse();
           if (terms) terms->traverse();
           spaces -= SPACES_ADD;
    }
};

struct Expression {
       struct AddSign *addsign;
       struct Term *term;
       struct Terms *terms;
       Expression() : addsign(nullptr), term(nullptr), terms(nullptr) {}
       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "Expression\n";
              spaces += SPACES_ADD;
           if (addsign) addsign->traverse();
           if (term) term->traverse();
           if (terms) terms->traverse();
           spaces -= SPACES_ADD;
    }
};

struct PrintStatement { 
    struct Expression *expression; 
    PrintStatement() : expression(nullptr) {}
    void traverse() {
           for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
           cout << "PrintStatement\n";
           spaces += SPACES_ADD;
           expression->traverse();
           spaces -= SPACES_ADD;
    }
};


main() {
	yyparse();
}


%}

%union {string ID; 
struct Factor* PNTFactor;
struct Term* PNTTerm;
struct Terms* PNTTerms;
struct Factors* PNTFactors;
struct Expression* PNTExpression;
struct AddSign* PNTAddsign;
struct MultSign* PNTMultSign;
struct PrintStatement* PNTPrintStatement;
struct CompoundName* PNTCompoundName;
struct LeftPart* PNTLeftPart;
struct NewType* PNTNewType;
struct ArrayTail* PNTArrayTail;
struct Type* PNTType;
}

// Identifiers & numbers 
%token <ID> IDENTIFIER NUMBER _NULL LBRACKET RBRACKET DOT

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
%token NEW
%token INT
%token REAL
// Delimiters


%token LBRACE      //  {
%token RBRACE      //  }
%token LPAREN      //  (
%token RPAREN      //  )
%token COMMA       //  ,
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

%type <PNTFactor> Factor
%type <PNTFactors> Factors
%type <PNTTerm> Term
%type <PNTTerms> Terms
%type <PNTExpression> Expression
%type <PNTAddsign> AddSign
%type <PNTMultSign> MultSign
%type <PNTCompoundName> CompoundName
%type <PNTLeftPart> LeftPart
%type <PNTNewType> NewType
%type <PNTType> Type
%type <PNTArrayTail> ArrayTail

%type <NUM> RelationalOperator Relation


%start PrintStatement

%%
CompilationUnit
       : Imports ClassDeclarations	
       ;

Imports
       :  /* empty */
       | Import Imports			
       ;

Import
       : IMPORT IDENTIFIER SEMICOLON	
       ;

ClassDeclarations
       : /* empty */
       | ClassDeclaration ClassDeclarations	
       ;

ClassDeclaration
       :        CLASS CompoundName Extension SEMICOLON ClassBody	
       | PUBLIC CLASS CompoundName Extension SEMICOLON ClassBody	
       ;

Extension
       : /* empty */			
       | EXTENDS IDENTIFIER		
       ;

ClassBody
       : LBRACE              RBRACE	
       | LBRACE ClassMembers RBRACE	
       ;

ClassMembers
       :              ClassMember	
       | ClassMembers ClassMember	
       ;

ClassMember
       : FieldDeclaration		
       | MethodDeclaration		
       ;

FieldDeclaration
       : Visibility Staticness Type IDENTIFIER SEMICOLON		
       ;

Visibility
       : /* empty */			
       | PRIVATE			
       | PUBLIC				
       ;

Staticness
       : /* empty */
       | STATIC				
       ;

MethodDeclaration
       : Visibility Staticness MethodType IDENTIFIER Parameters		
            Body			
       ;

Parameters
       : LPAREN               RPAREN	
       | LPAREN ParameterList RPAREN	
       ;

ParameterList
       :                     Parameter	
       | ParameterList COMMA Parameter	
       ;

Parameter
       : Type IDENTIFIER		
       ;

MethodType
       : Type				
       | VOID				
       ;

Body
       : LBRACE LocalDeclarations Statements RBRACE	
       ;

LocalDeclarations
       :                   LocalDeclaration		
       | LocalDeclarations LocalDeclaration		
       ;

LocalDeclaration
       : Type IDENTIFIER SEMICOLON			
       ;

Statements
       :            Statement		
       | Statements Statement		
       ;

Statement
       : Assignment			
       | IfStatement			
       | WhileStatement			
       | ReturnStatement		
       | CallStatement			
       | PrintStatement			
       | Block				
       ;

Assignment
       : LeftPart ASSIGN Expression SEMICOLON		
       ;

LeftPart
       : CompoundName                                   {struct LeftPart *lp = new LeftPart(); lp->compoundname = $1; $$ = lp;}
       | CompoundName LBRACKET Expression RBRACKET      {struct LeftPart *lp = new LeftPart(); lp->compoundname = $1; lp->t_lbracket = $2; lp->expression = $3; lp->t_rbracket = $4; $$ = lp;}
       ;

CompoundName
       :                  IDENTIFIER                    {struct CompoundName *cp = new CompoundName(); cp->t_id = $1; $$ = cp;}
       | CompoundName DOT IDENTIFIER			{struct CompoundName *cp = new CompoundName(); cp->compoundname = $1; cp->t_dot = $2; cp->t_id = $3; $$ = cp;}
       ;

IfStatement
       : IF LPAREN Relation RPAREN Statement		
       | IF LPAREN Relation RPAREN Statement ELSE Statement	
       ;

WhileStatement
       : WHILE Relation LOOP Statement SEMICOLON	
       ;

ReturnStatement
       : RETURN            SEMICOLON	{return;}
       | RETURN Expression SEMICOLON	{return $2;}
       ;

CallStatement
       : CompoundName LPAREN              RPAREN SEMICOLON	
       | CompoundName LPAREN ArgumentList RPAREN SEMICOLON	
       ;

ArgumentList
       :                    Expression	{;}
       | ArgumentList COMMA Expression	
       ;

PrintStatement
       : PRINT Expression SEMICOLON	{struct PrintStatement *ps = new PrintStatement(); ps->expression = $2; ps->traverse();}
       ;

Block
       : LBRACE            RBRACE	
       | LBRACE Statements RBRACE	
       ;

Relation
       : Expression			{$$ = (int)($1 != 0);}
       | Expression RelationalOperator Expression       {$$ = relation($1, $3, $2);}
       ;

RelationalOperator
       : LESS				{$$ = 1;}
       | GREATER			{$$ = 2;}
       | EQUAL			{$$ = 3;}
       | NOT_EQUAL			{$$ = 4;}
       ;

Expression
       :         Term Terms		{struct Expression *ex = new Expression(); ex->addsign = nullptr; ex->term = $1; ex->terms = $2; $$ = ex;}
       | AddSign Term Terms		{struct Expression *ex = new Expression(); ex->addsign = $1; ex->term = $2; ex->terms = $3; $$ = ex;}
       ;

AddSign
       : PLUS				{struct AddSign *as = new AddSign(); as->addsign = "+"; $$ = as;}
       | MINUS			{struct AddSign *as = new AddSign(); as->addsign = "-"; $$ = as;}
       ;

Terms
       : /* empty */			{$$ = nullptr;}
       | AddSign Term Terms		{struct Terms *ts = new Terms(); ts->addsign = $1; ts->term = $2; ts->terms = $3; $$ = ts;}
       ;

Term
       : Factor                    {struct Term *t = new Term(); t->factor = $1; t->factors = nullptr; $$ = t;}
       | Factor Factors            {struct Term *t = new Term(); t->factor = $1; t->factors = $2; $$ = t;}
       ;

Factors
       : /* empty */			{$$ = nullptr;}
       | MultSign Factor Factors	{struct Factors *fs = new Factors(); fs->multsign = $1; fs->factor = $2; fs->factors = $3; $$ = fs;}
       ;

MultSign
       : MULTIPLY			{struct MultSign *ms = new MultSign(); ms->multsign = "*"; $$ = ms;}
       | DIVIDE			{struct MultSign *ms = new MultSign(); ms->multsign = "/"; $$ = ms;}
       ;

Factor
       : NUMBER		                     	{struct Factor *f = new Factor(); f->t_number = $1; $$ = f;}
       | LeftPart	                        		{struct Factor *f = new Factor(); f->leftpart = $1; $$ = f;}
       | _NULL	                     		{struct Factor *f = new Factor(); f->t_null = $1; $$ = f;}
       | NEW NewType	                     		{struct Factor *f = new Factor(); f->t_new = $1; f->newtype = $2; $$ = f}
       | NEW NewType LBRACKET Expression RBRACKET	{struct Factor *f = new Factor(); f->t_new = $1; f->newtype = $2; f->t_lbracket = $3; f->expression = $4; f->t_rbracket = $5; $$ = f;}
       ;

NewType
       : INT				{struct NewType *nt = new NewType(); nt->token = $1; $$ = nt;}
       | REAL				{struct NewType *nt = new NewType(); nt->token = $1; $$ = nt;}
       | IDENTIFIER			{struct NewType *nt = new NewType(); nt->token = $1; $$ = nt;}
       ;

Type
       : INT        ArrayTail		{struct Type *t = new Type(); t->token = $1; t->arraytail = $2; $$ = t;}
       | REAL       ArrayTail		{struct Type *t = new Type(); t->token = $1; t->arraytail = $2; $$ = t;}
       | IDENTIFIER ArrayTail		{struct Type *t = new Type(); t->token = $1; t->arraytail = $2; $$ = t;}
       ;

ArrayTail
       : /* empty */
       | LBRACKET RBRACKET		{struct ArrayTail *at = new ArrayTail(); at->t_lbracket = $1; at->t_rbracket = $2; $$ = at;}
       ;

%%
