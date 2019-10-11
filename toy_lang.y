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
       if (!token) return;
       for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
       cout << token << "\n";
}

struct Extension {
       string token1, token2;
       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "ClassBody\n";
              spaces += SPACES_ADD;
              print_token(token1);
              print_token(token2);
              spaces -= SPACES_ADD;
       }
};

struct ClassBody {
       string t_lbrace, t_rbrace;
       struct ClassMembers *classmembers;
       ClassBody() : classmembers(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "ClassBody\n";
              spaces += SPACES_ADD;
              print_token(t_lbrace);
              if (classmembers) classmembers->traverse();
              print_token(t_rbrace);
              spaces -= SPACES_ADD;
       }
};

struct ClassMembers {
       struct ClassMembers *classmembers;
       struct ClassMember *classmember;

       ClassMembers() : classmembers(nullptr), classmember(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "ClassMembers\n";
              spaces += SPACES_ADD;
              if (classmembers) classmembers->traverse();
              if (classmember) classmember->traverse();
              spaces -= SPACES_ADD;
       }
};

struct ClassMember {
       struct FieldDeclaration *fielddeclaration;
       struct MethodDeclaration *methoddeclaration;
       ClassMember() : fielddeclaration(nullptr), methoddeclaration(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "ClassMember\n";
              spaces += SPACES_ADD;
              if (fielddeclaration) fielddeclaration->traverse();
              if (methoddeclaration) methoddeclaration->traverse();
              spaces += SPACES_ADD;
       }
};

struct FieldDeclaration {
       struct Visibility *visibility;
       struct Staticness *staticness;
       struct Type *type;
       string t_id, t_semicolon;

       FieldDeclaration() : visibility(nullptr), staticness(nullptr), type(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "FieldDeclaration\n";
              spaces += SPACES_ADD;

              if (visibility) visibility->traverse();
              if (staticness) staticness->traverse();
              if (type) type->traverse();
              print_token(t_id);
              print_token(t_semicolon);

              spaces -= SPACES_ADD;
       }
};

struct Visibility {
       string token;

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "Visibility\n";
              spaces += SPACES_ADD;
              print_token(token);
              spaces -= SPACES_ADD;
       }
};

struct Staticness {
       string t_static;
       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "Staticness\n";
              spaces += SPACES_ADD;
              print_token(t_static);
              spaces -= SPACES_ADD;
       }
};

struct MethodDeclaration {
       struct Visibility *visibility;
       struct Staticness *staticness;
       struct MethodType *methodtype;
       struct Parameters *parameters;
       struct Body *body;
       string t_id;
       MethodDeclaration() : visibility(nullptr), staticness(nullptr), methodtype(nullptr), parameters(nullptr), body(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "MethodDeclaration\n";
              spaces += SPACES_ADD;
              if (visibility) visibility->traverse();
              if (staticness) staticness->traverse();
              if (methodtype) methodtype->traverse();
              print_token(t_id);
              if (parameters) parameters->traverse();
              if (body) body->traverse();
              spaces -= SPACES_ADD;
       }
};

struct Parameters {
       struct ParameterList *parameterlist;
       string t_lparen, t_rparen;

       ParameterList() : parameterlist(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "ParameterList\n";
              spaces += SPACES_ADD;
              print_token(t_lparen);
              if (parameterlist) parameterlist->traverse();
              print_token(t_rparen);
              spaces -= SPACES_ADD;
       }
};

struct ParameterList {
       struct Parameter *parameter;
       struct ParameterList *parameterlist;
       string t_comma;

       ParameterList() : parameterlist(nullptr), parameterlist(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "ParameterList\n";
              spaces += SPACES_ADD;
              if (parameterlist) parameterlist->traverse();
              print_token(t_comma);
              if (parameter) parameter->traverse();
              spaces -= SPACES_ADD;
       }
};

struct Parameter {
       struct Type *type;
       string t_id;
       Parameter() : type(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "Parameter\n";
              spaces += SPACES_ADD;
              if (type) type->traverse();
              print_token(t_id);
              spaces -= SPACES_ADD;
       }
};

struct MethodType {
       struct Type *type;
       string t_void;

       MethodType() : type(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "MethodType\n";
              spaces += SPACES_ADD;
              if (type) type->traverse;
              print_token(t_void);
              spaces -= SPACES_ADD;
       }
};

struct Body { 
       string t_lbrace, t_rbrace;
       struct LocalDeclarations *localdeclarations;
       struct Statements *statements;

       Body() : localdeclarations(nullptr), statements(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "Body\n";
              spaces += SPACES_ADD;
              print_token(t_lbrace);
              if (localdeclarations) localdeclarations->traverse();
              if (statements) statements->traverse();
              print_token(t_rbrace);
              spaces -= SPACES_ADD;
       }

};

struct LocalDeclarations {
       struct LocalDeclarations *localdeclarations;
       struct LocalDeclaration *localdeclaration;
       LocalDeclarations() : localdeclaration(nullptr), localdeclarations(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "LocalDeclaration\n";
              spaces += SPACES_ADD;
              if (localdeclarations) localdeclarations->traverse();
              if (localdeclaration) localdeclaration->traverse();
              spaces -= SPACES_ADD;
       }
};

struct LocalDeclaration {
       struct Type *type;
       string t_id, t_semicolon;
       LocalDeclaration() : type(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "LocalDeclaration\n";
              spaces += SPACES_ADD;
              if (type) type->traverse();
              print_token(t_id);
              print_token(t_semicolon);
              spaces -= SPACES_ADD;
       }
};

struct Statements {
       struct Statement *statement;
       struct Statements *statements;
       Statements() : statement(nullptr), statements(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "Statement\n";
              spaces += SPACES_ADD;
              if (statements) statements->traverse();
              if (statement) statement->traverse();
              spaces -= SPACES_ADD;
       }
};

struct Statement {
       struct Assignment *assignment;
       struct IfStatement *ifstatement;
       struct WhileStatement *whilestatement;
       struct ReturnStatement *returnstatement;
       struct CallStatement *callstatement;
       struct PrintStatement *printstatement;
       struct Block *block;
       Statement() : assignment(nullptr), ifstatement(nullptr), whilestatement(nullptr), returnstatement(nullptr), callstatement(nullptr), printstatement(nullptr), block(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "Statement\n";
              spaces += SPACES_ADD;

              if (assignment) assignment->traverse();
              if (ifstatement) ifstatement->traverse();
              if (whilestatement) whilestatement->traverse();
              if (returnstatement) returnstatement->traverse();
              if (callstatement) callstatement->traverse();
              if (printstatement) printstatement->traverse();
              if (block) block->traverse();

              spaces -= SPACES_ADD;
       }
};

struct Assignment {
       struct LeftPart *leftpart;
       struct Expression *expression;
       string t_assign, t_semicolon;
       Assignment() : leftpart(nullptr), expression(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "Assignment\n";
              spaces += SPACES_ADD;
              if (leftpart) leftpart->traverse();
              print_token(t_assign);
              if (expression) expression->traverse();
              print_token(t_semicolon);
              spaces -= SPACES_ADD;
       }
};

struct IfStatement {
       string t_if, t_lparen, t_rparen, t_else;
       struct Relation *relation;
       struct Statement * statement1, statement2;
       IfStatement() : relation(nullptr), statement1(nullptr), statement2(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "WhileStatement\n";
              spaces += SPACES_ADD;
              print_token(t_if);
              print_token(t_lparen);
              if (relation) relation->traverse();
              print_token(t_rparen);
              if (statement1) statement1->traverse();
              print_token(t_else);
              if (statement2) statement2->traverse();
              spaces -= SPACES_ADD;
       }
};

struct WhileStatement {
       string t_while, t_loop, t_semicolon;
       struct Relation *relation;
       struct Statement *statement;
       WhileStatement() : relationaloperator(nullptr), statement(nullptr) {}
       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "WhileStatement\n";
              spaces += SPACES_ADD;

              print_token(t_while);
              if (relation) relation->traverse();
              print_token(t_loop);
              if (statement) statement->traverse();
              print_token(t_semicolon);

              spaces -= SPACES_ADD;
       }
}

struct ReturnStatement {
       string t_return, t_semicolon
       struct Expression *expression;
       ReturnStatement() : expression(nullptr) {}
       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "ReturnStatement\n";
              spaces += SPACES_ADD;
              if (t_return) print_token(t_return);
              if (expression) expression->traverse();
              if (t_semicolon) print_token(t_semicolon);
              spaces -= SPACES_ADD;
       }
};

struct CallStatement {
       string t_lparen, t_rparen, t_semicolon;
       struct CompoundName *compoundname;
       struct ArgumentList *argumentlist;

       CallStatement() : compoundname(nullptr), argumentlist(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "CallStatement\n";
              spaces += SPACES_ADD;
              if (compoundname) compoundname->traverse();
              if (t_lparen) print_token(t_lparen);
              if (argumentlist) argumentlist->traverse();
              if (t_rparen) print_token(t_rparen);
              if (t_semicolon) print_token(t_semicolon);
              spaces -= SPACES_ADD;
       }
};

struct ArgumentList {
       struct ArgumentList *argumentlist;
       string t_comma;
       struct Expression *expression;

       ArgumentList() : argumentlist(nullptr), expression(nullptr) {}
       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "ArgumentList\n";
              spaces += SPACES_ADD;
              if (argumentlist) argumentlist->traverse();
              if (t_comma) print_token(t_comma);
              if (expression) expression->traverse();
              spaces -= SPACES_ADD;
       }
};

struct Block {
       struct Statements * statements;
       string t_lbrace, t_rbrace;
       Block() : statements(nullptr) {}

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "Block\n";
              spaces += SPACES_ADD;
              if (t_lbrace) print_token(t_lbrace);
              if (statements) statements->traverse();
              if (t_rbrace) print_token(t_rbrace);
              spaces -= SPACES_ADD;
       }
};

struct Relation {
       struct Expression *expression1, expression2;
       struct RelationalOperator * relationaloperator;
       Relation() : expression1(nullptr), expression2(nullptr), relationaloperator(nullptr) {}
       
       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "Relation\n";
              spaces += SPACES_ADD;

              if (expression1) expression1->traverse()
              if (relationaloperator) relationaloperator->traverse();
              if (expression2) expression2->traverse();

              spaces -= SPACES_ADD;
       }
};

struct RelationalOperator {
     string token;

       void traverse() {
              for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
              cout << "RelationalOperator\n";
              spaces += SPACES_ADD;
              print_token(token);
              spaces -= SPACES_ADD;
       }  
};

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
       | EXTENDS IDENTIFIER		{struct Extension *e = new Extension(); e->token1 = $1; e->token2 = $2; $$ = e;}
       ;

ClassBody
       : LBRACE              RBRACE	{struct ClassBody *cb = new ClassBody(); cb->t_lbrace = $1; cb->t_rbrace = $2; $$ = cb;}
       | LBRACE ClassMembers RBRACE	{struct ClassBody *cb = new ClassBody(); cb->t_lbrace = $1; cb->classmember = $2; cb->t_rbrace = $3; $$ = cb;}
       ;

ClassMembers
       :              ClassMember	 {struct ClassMembers *cm = new ClassMembers(); cm->classmember = $1; $$ = cm;}
       | ClassMembers ClassMember	 {struct ClassMembers *cm = new ClassMembers(); cd->classmembers = $2; cm->classmember = $2; $$ = cm;}
       ;

ClassMember
       : FieldDeclaration		{struct ClassMember *cm = new ClassMember(); cm->fielddeclaration = $1; $$ = cm;}
       | MethodDeclaration		{struct ClassMember *cm = new ClassMember(); cm->methoddeclaration = $1; $$ = cm;}
       ;

FieldDeclaration
       : Visibility Staticness Type IDENTIFIER SEMICOLON	{struct FieldDeclaration *fd = new FieldDeclaration(); fd->visibility = $1; fd->staticness = $2; fd->type = $3; fd->t_id = $4; fd->t_semicolon = $5; $$ = fd;}
       ;

Visibility
       : /* empty */			
       | PRIVATE			{struct Visibility *v = new Visibility(); v->token = $1; $$ = v;}
       | PUBLIC			{struct Visibility *v = new Visibility(); v->token = $1; $$ = v;}
       ;

Staticness
       : /* empty */
       | STATIC				{struct Staticness *s = new Staticness(); s->t_static = $1; $$ = s;}
       ;

MethodDeclaration
       : Visibility Staticness MethodType IDENTIFIER Parameters Body {struct MethodDeclaration *md = new MethodDeclaration(); md->visibility = $1; md->staticness = $2; md->methodtype = $3; md->t_id = $4; md->parameters = $5; md->body = $6; $$ = md;}
       ;

Parameters
       : LPAREN               RPAREN	{struct Parameters *ps = new Parameters(); ps->t_lparen = $1; ps->t_rparen = $2; $$ = ps;}
       | LPAREN ParameterList RPAREN	{struct Parameters *ps = new Parameters(); ps->t_lparen = $1; ps->parameterlist  $2; ps->t_rparen = $3; $$ = ps;}
       ;

ParameterList
       :                     Parameter	{struct ParameterList *pl = new ParameterList(); pl->parameter = $1; $$ = pl;}
       | ParameterList COMMA Parameter	{struct ParameterList *pl = new ParameterList(); pl->parameterlist = $1; pl->t_comma = $2; pl->parameter = $3; $$ = pl;}
       ;

Parameter
       : Type IDENTIFIER		{struct Parameter *p = new Parameter(); p->type = $1; p->t_id = $2; $$ = p;}
       ;

MethodType
       : Type				{struct MethodType *mt = new MethodType(); mt->type = $1; $$ = mt;}
       | VOID				{struct MethodType *mt = new MethodType(); mt->t_void = $1; $$ = mt;}
       ;

Body
       : LBRACE LocalDeclarations Statements RBRACE	{struct Body *b = new Body(); b->t_lbrace = $1; b->localdeclarations = $2; b->statements = $3; b->t_rbrace = $4; $$ = b;}
       ;

LocalDeclarations
       :                   LocalDeclaration	{struct LocalDeclarations *ld = new LocalDeclarations(); ld->localdeclaration = $1; $$ = ld;}
       | LocalDeclarations LocalDeclaration	{struct LocalDeclarations *ld = new LocalDeclarations(); ls->localdeclarations = $1; ld->localdeclaration = $2; $$ = ld;}
       ;

LocalDeclaration
       : Type IDENTIFIER SEMICOLON		{struct LocalDeclaration *ld = new LocalDeclaration(); ld->type = &1; ld->t_id = $2; ld->t_semicolon = $3; $$ = ld;}
       ;

Statements
       :            Statement		{struct Statements *s = new Statements(); s->statement = $1; $$ = s;}
       | Statements Statement		{struct Statements *s = new Statements(); s->statements = $1; s->statement = $2; $$ = s;}
       ;

Statement
       : Assignment			{struct Statement *s = new Statement(); s->assignment = $1; $$ = s;}
       | IfStatement			{struct Statement *s = new Statement(); s->ifstatement = $1; $$ = s;}
       | WhileStatement		{struct Statement *s = new Statement(); s->whilestatement = $1; $$ = s;}	
       | ReturnStatement		{struct Statement *s = new Statement(); s->returnstatement = $1; $$ = s;}
       | CallStatement		{struct Statement *s = new Statement(); s->callstatement = $1; $$ = s;}	
       | PrintStatement		{struct Statement *s = new Statement(); s->printstatement = $1; $$ = s;}	
       | Block			{struct Statement *s = new Statement(); s->block = $1; $$ = s;}	
       ;

Assignment
       : LeftPart ASSIGN Expression SEMICOLON		{struct Assignment *a = new Assignment(); a->leftpart = $1; a->t_assign = $2; a->expression = $3; a->t_semicolon = $4; $$ = a;}
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
       : IF LPAREN Relation RPAREN Statement		       {struct IfStatement *is = new IfStatement(); is->t_if = $1; is->t_lparen = $2; t->relation = $3; t->RPAREN = $4; t->statement1 = $5; $$ = t;}
       | IF LPAREN Relation RPAREN Statement ELSE Statement	{struct IfStatement *is = new IfStatement(); is->t_if = $1; is->t_lparen = $2; t->relation = $3; t->RPAREN = $4; t->statement1 = $5; t->t_else = $6; t->statement2 = $7; $$ = t;}
       ;

WhileStatement
       : WHILE Relation LOOP Statement SEMICOLON	{struct WhileStatement *ws = new WhileStatement(); ws->t_while = $1; ws->relation = $2; ws->t_loop = $3; ws->statement = $4; ws->t_semicolon = $5; $$ = ws;}
       ;

ReturnStatement
       : RETURN            SEMICOLON	{struct ReturnStatement rs = new ReturnStatement(); rs->t_return = $1; rs->t_semicolon = $2; $$ = rs;}
       | RETURN Expression SEMICOLON	{struct ReturnStatement rs = new ReturnStatement(); rs->t_return = $1; rs->expression = $2; rs->t_semicolon = $3; $$ = rs;}
       ;

CallStatement
       : CompoundName LPAREN              RPAREN SEMICOLON	{struct CallStatement *cs = new CallStatement(); cs->compoundname = $1; cd->t_lparen = $2; cs->t_rparen = $3; cs->t_semicolon = $4; $$ = cs;}
       | CompoundName LPAREN ArgumentList RPAREN SEMICOLON	{struct CallStatement *cs = new CallStatement(); cs->compoundname = $1; cd->t_lparen = $2; cd->argumentlist = $3; cs->t_rparen = $4; cs->t_semicolon = $5; $$ = cs;}
       ;

ArgumentList
       :                    Expression	{struct ArgumentList *al = new ArgumentList(); ar->expression = $1; $$ = ar;}
       | ArgumentList COMMA Expression	{struct ArgumentList *al = new ArgumentList(); ar->argumentlist = $1; ar->t_comma = $2; ar->expression = $3; $$ = ar;}
       ;

PrintStatement
       : PRINT Expression SEMICOLON	{struct PrintStatement *ps = new PrintStatement(); ps->expression = $2; ps->traverse();}
       ;

Block
       : LBRACE            RBRACE	       {struct Block *b = new Block(); b->t_lbrace = $1; b->t_rbrace = $2; $$ = b;}
       | LBRACE Statements RBRACE         {struct Block *b = new Block(); b->t_lbrace = $1; b->statements = $2; b->t_rbrace = $3; $$ = b;}
       ;

Relation
       : Expression			                     {struct Relation *r = new Relation(); r->expression1 = $1; $$ = r;}
       | Expression RelationalOperator Expression       {struct Relation *r = new Relation(); r->expression1 = $1; r->relationaloperator = $2; r->expression2 = $3; $$ = r;}
       ;

RelationalOperator
       : LESS				{struct RelationalOperator *ro = new RelationalOperator(); ro->token = $1; $$ = ro;}
       | GREATER			{struct RelationalOperator *ro = new RelationalOperator(); ro->token = $1; $$ = ro;}
       | EQUAL			{struct RelationalOperator *ro = new RelationalOperator(); ro->token = $1; $$ = ro;}
       | NOT_EQUAL			{struct RelationalOperator *ro = new RelationalOperator(); ro->token = $1; $$ = ro;}
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
