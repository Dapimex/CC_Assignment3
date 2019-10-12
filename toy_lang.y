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


int spaces = 0;

void print_token(string token) {
       if (token == "") return;
       for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
       cout << token << "\n";
}

struct CompilationUnit {
       struct Imports *imports;
       struct ClassDeclarations *classdeclarations;

       CompilationUnit() : imports(nullptr), classdeclarations(nullptr) {}

};

struct Imports {
       struct Import *import;
       struct Imports *imports;
       Imports() : import(nullptr), imports(nullptr) {}

};

struct Import {
       string token1, token2, token3;
};

struct ClassDeclarations {
       struct ClassDeclaration *classdeclaration;
       struct ClassDeclarations *classdeclarations;
       
       ClassDeclarations() : classdeclaration(nullptr), classdeclarations(nullptr) {}

};

struct ClassDeclaration {
       string t_class, t_public, t_semicolon;
       struct CompoundName *compoundname;
       struct Extension *extension;
       struct ClassBody *classbody;

       ClassDeclaration() : compoundname(nullptr), extension(nullptr), classbody(nullptr) {}

};

struct Extension {
       string token1, token2;
};

struct ClassBody {
       string t_lbrace, t_rbrace;
       struct ClassMembers *classmembers;
       ClassBody() : classmembers(nullptr) {}

};

struct ClassMembers {
       struct ClassMembers *classmembers;
       struct ClassMember *classmember;

       ClassMembers() : classmembers(nullptr), classmember(nullptr) {}

};

struct ClassMember {
       struct FieldDeclaration *fielddeclaration;
       struct MethodDeclaration *methoddeclaration;
       ClassMember() : fielddeclaration(nullptr), methoddeclaration(nullptr) {}

};

struct FieldDeclaration {
       struct Visibility *visibility;
       struct Staticness *staticness;
       struct Type *type;
       string t_id, t_semicolon;

       FieldDeclaration() : visibility(nullptr), staticness(nullptr), type(nullptr) {}

};

struct Visibility {
       string token;

};

struct Staticness {
       string t_static;
};

struct MethodDeclaration {
       struct Visibility *visibility;
       struct Staticness *staticness;
       struct MethodType *methodtype;
       struct Parameters *parameters;
       struct Body *body;
       string t_id;
       MethodDeclaration() : visibility(nullptr), staticness(nullptr), methodtype(nullptr), parameters(nullptr), body(nullptr) {}

};

struct Parameters {
       struct ParameterList *parameterlist;
       string t_lparen, t_rparen;

       Parameters() : parameterlist(nullptr) {}

};

struct ParameterList {
       struct Parameter *parameter;
       struct ParameterList *parameterlist;
       string t_comma;

       ParameterList() : parameter(nullptr), parameterlist(nullptr) {}

};

struct Parameter {
       struct Type *type;
       string t_id;
       Parameter() : type(nullptr) {}

};

struct MethodType {
       struct Type *type;
       string t_void;

       MethodType() : type(nullptr) {}

};

struct Body { 
       string t_lbrace, t_rbrace;
       struct LocalDeclarations *localdeclarations;
       struct Statements *statements;

       Body() : localdeclarations(nullptr), statements(nullptr) {}

};

struct LocalDeclarations {
       struct LocalDeclarations *localdeclarations;
       struct LocalDeclaration *localdeclaration;
       LocalDeclarations() : localdeclaration(nullptr), localdeclarations(nullptr) {}

};

struct LocalDeclaration {
       struct Type *type;
       string t_id, t_semicolon;
       LocalDeclaration() : type(nullptr) {}

};

struct Statements {
       struct Statement *statement;
       struct Statements *statements;
       Statements() : statement(nullptr), statements(nullptr) {}

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

};

struct Assignment {
       struct LeftPart *leftpart;
       struct Expression *expression;
       string t_assign, t_semicolon;
       Assignment() : leftpart(nullptr), expression(nullptr) {}

};

struct IfStatement {
       string t_if, t_lparen, t_rparen, t_else;
       struct Relation *relation;
       struct Statement * statement1, *statement2;
       IfStatement() : relation(nullptr), statement1(nullptr), statement2(nullptr) {}

};

struct WhileStatement {
       string t_while, t_loop, t_semicolon;
       struct Relation *relation;
       struct Statement *statement;
       WhileStatement() : relation(nullptr), statement(nullptr) {}
};

struct ReturnStatement {
       string t_return, t_semicolon;
       struct Expression *expression;
       ReturnStatement() : expression(nullptr) {}
};

struct CallStatement {
       string t_lparen, t_rparen, t_semicolon;
       struct CompoundName *compoundname;
       struct ArgumentList *argumentlist;

       CallStatement() : compoundname(nullptr), argumentlist(nullptr) {}

};

struct ArgumentList {
       struct ArgumentList *argumentlist;
       string t_comma;
       struct Expression *expression;

       ArgumentList() : argumentlist(nullptr), expression(nullptr) {}
};

struct Block {
       struct Statements * statements;
       string t_lbrace, t_rbrace;
       Block() : statements(nullptr) {}

};

struct Relation {
       struct Expression *expression1, *expression2;
       struct RelationalOperator * relationaloperator;
       Relation() : expression1(nullptr), expression2(nullptr), relationaloperator(nullptr) {}
       
};

struct RelationalOperator {
     string token;

};

struct ArrayTail {
       string t_lbracket, t_rbracket;
};

struct Type {
       string token;
       struct ArrayTail *arraytail;
       
       Type() : arraytail(nullptr) {}

};

struct CompoundName {
       string t_dot, t_id;
       struct CompoundName *compoundname;
       CompoundName() : compoundname(nullptr) {};

};

struct LeftPart {
       string t_lbracket, t_rbracket;
       struct Expression *expression;
       struct CompoundName *compoundname;
       LeftPart() : expression(nullptr), compoundname(nullptr) {}

};

struct NewType {
       string token;
};

struct Factor {
    string t_number, t_new, t_null, t_lbracket, t_rbracket;
    struct Expression *expression;
    struct NewType *newtype;
    struct LeftPart *leftpart;

    Factor() : expression(nullptr), newtype(nullptr) {}

};

struct MultSign {
    string multsign;
};

struct Factors {
    struct MultSign *multsign;
    struct Factor *factor;
    struct Factors *factors;
    Factors() : multsign(nullptr), factor(nullptr), factors(nullptr) {}
};

struct Term {
    struct Factor *factor;
    struct Factors *factors;
    Term() : factor(nullptr), factors(nullptr) {}
};

struct AddSign {
    string addsign;
};

struct Terms {
    struct AddSign *addsign;
    struct Term *term;
    struct Terms *terms;
    Terms() : addsign(nullptr), term(nullptr), terms(nullptr) {}
};

struct Expression {
       struct AddSign *addsign;
       struct Term *term;
       struct Terms *terms;
       Expression() : addsign(nullptr), term(nullptr), terms(nullptr) {}
};

struct PrintStatement { 
       string t_semicolon, t_print;
    struct Expression *expression; 
    PrintStatement() : expression(nullptr) {}
};

void compilationunit_traverse(CompilationUnit* cur);
void imports_traverse(Imports* cur);
void import_traverse(Import* cur);
void classdeclarations_traverse(ClassDeclarations* cur);
void classdeclaration_traverse(ClassDeclaration* cur);
void extension_traverse(Extension* cur);
void classbody_traverse(ClassBody* cur);
void classmembers_traverse(ClassMembers* cur);
void classmember_traverse(ClassMember* cur);
void fielddeclaration_traverse(FieldDeclaration* cur);
void visibility_traverse(Visibility* cur);
void staticness_traverse(Staticness* cur);
void methoddeclaration_traverse(MethodDeclaration* cur);
void parameters_traverse(Parameters* cur);
void parameterlist_traverse(ParameterList* cur);
void parameter_traverse(Parameter* cur);
void methodtype_traverse(MethodType* cur);
void body_traverse(Body* cur);
void localdeclarations_traverse(LocalDeclarations* cur);
void localdeclaration_traverse(LocalDeclaration* cur);
void statements_traverse(Statements* cur);
void statement_traverse(Statement* cur);
void assignment_traverse(Assignment* cur);
void ifstatement_traverse(IfStatement* cur);
void whilestatement_traverse(WhileStatement* cur);
void returnstatement_traverse(ReturnStatement* cur);
void callstatement_traverse(CallStatement* cur);
void argumentlist_traverse(ArgumentList* cur);
void block_traverse(Block* cur);
void relation_traverse(Relation* cur);
void relationaloperator_traverse(RelationalOperator* cur);
void arraytail_traverse(ArrayTail* cur);
void type_traverse(Type* cur);
void compoundname_traverse(CompoundName* cur);
void leftpart_traverse(LeftPart* cur);
void newtype_traverse(NewType* cur);
void factor_traverse(Factor* cur);
void multsign_traverse(MultSign* cur);
void factors_traverse(Factors* cur);
void term_traverse(Term* cur);
void addsign_traverse(AddSign* cur);
void terms_traverse(Terms* cur);
void expression_traverse(Expression* cur);
void printstatement_traverse(PrintStatement* cur);


main() {
	yyparse();
}


%}

%union {char* ID; 
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
struct Type* PNTType;

struct ArrayTail* PNTArrayTail;
struct RelationalOperator* PNTRelationalOperator;
struct Relation* PNTRelation;
struct Block* PNTBlock;
struct ArgumentList* PNTArgumentList;
struct CallStatement* PNTCallStatement;
struct ReturnStatement* PNTReturnStatement;
struct WhileStatement* PNTWhileStatement;
struct IfStatement* PNTIfStatement;
struct Assignment* PNTAssignment;
struct Statement* PNTStatement;
struct Statements* PNTStatements;
struct LocalDeclaration* PNTLocalDeclaration;
struct LocalDeclarations* PNTLocalDeclarations;
struct Body* PNTBody;
struct MethodType* PNTMethodType;
struct Parameter* PNTParameter;
struct ParameterList* PNTParameterList;
struct Parameters* PNTParameters;
struct MethodDeclaration* PNTMethodDeclaration;
struct Staticness* PNTStaticness;
struct Visibility* PNTVisibility;
struct FieldDeclaration* PNTFieldDeclaration;
struct ClassMember* PNTClassMember;
struct ClassMembers* PNTClassMembers;
struct ClassBody* PNTClassBody;
struct Extension* PNTExtension;
struct ClassDeclaration* PNTClassDeclaration;
struct ClassDeclarations* PNTClassDeclarations;
struct Import* PNTImport;
struct Imports* PNTImports;
struct CompilationUnit* PNTCompilationUnit;



}

// Identifiers & numbers and Keywords
%token <ID> IDENTIFIER NUMBER 
%token <ID> _NULL LBRACKET RBRACKET DOT IMPORT CLASS EXTENDS PRIVATE STATIC PUBLIC VOID IF ELSE WHILE LOOP RETURN PRINT NEW INT REAL

// Delimiters
%token <ID> LBRACE RBRACE LPAREN RPAREN COMMA SEMICOLON

// Operator signs
%token <ID> ASSIGN LESS GREATER EQUAL NOT_EQUAL PLUS MINUS MULTIPLY DIVIDE

%token EOFF

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
%type <PNTRelationalOperator> RelationalOperator
%type <PNTRelation> Relation
%type <PNTBlock> Block
%type <PNTArgumentList> ArgumentList
%type <PNTCallStatement> CallStatement
%type <PNTReturnStatement> ReturnStatement
%type <PNTWhileStatement> WhileStatement
%type <PNTIfStatement> IfStatement
%type <PNTAssignment> Assignment
%type <PNTStatement> Statement
%type <PNTStatements> Statements
%type <PNTLocalDeclaration> LocalDeclaration
%type <PNTLocalDeclarations> LocalDeclarations
%type <PNTBody> Body
%type <PNTMethodType> MethodType
%type <PNTParameter> Parameter
%type <PNTParameterList> ParameterList
%type <PNTParameters> Parameters
%type <PNTMethodDeclaration> MethodDeclaration
%type <PNTStaticness> Staticness
%type <PNTVisibility> Visibility
%type <PNTFieldDeclaration> FieldDeclaration
%type <PNTClassMember> ClassMember
%type <PNTClassMembers> ClassMembers
%type <PNTClassBody> ClassBody
%type <PNTExtension> Extension
%type <PNTClassDeclaration> ClassDeclaration
%type <PNTClassDeclarations> ClassDeclarations
%type <PNTImport> Import
%type <PNTImports> Imports
%type <PNTCompilationUnit> CompilationUnit
%type <PNTPrintStatement> PrintStatement


%start CompilationUnit

%%
CompilationUnit
       : Imports ClassDeclarations        {struct CompilationUnit *cu = new CompilationUnit(); cu->imports = $1; cu->classdeclarations = $2; compilationunit_traverse(cu); $$ = cu;}
       ;

Imports
       :  /* empty */                     {$$ = nullptr;}
       | Import Imports			{struct Imports *is = new Imports(); is->import = $1; is->imports = $2; $$ = is;}
       ;

Import
       : IMPORT IDENTIFIER SEMICOLON	{struct Import *i = new Import(); i->token1 = $1; i->token2 = $2; i->token3 = $3; $$ = i;}
       ;

ClassDeclarations
       :                              {$$ = nullptr;}
       | ClassDeclaration ClassDeclarations	{struct ClassDeclarations *cd = new ClassDeclarations(); cd->classdeclaration = $1; cd->classdeclarations = $2; $$ = cd;}
       ;

ClassDeclaration
       :        CLASS CompoundName Extension SEMICOLON ClassBody	{struct ClassDeclaration *cd = new ClassDeclaration(); cd->t_class = $1; cd->compoundname = $2; cd->extension = $3; cd->t_semicolon = $4; cd->classbody = $5; $$ = cd;}
       | PUBLIC CLASS CompoundName Extension SEMICOLON ClassBody	{struct ClassDeclaration *cd = new ClassDeclaration(); cd->t_public = $1; cd->t_class = $2; cd->compoundname = $3; cd->extension = $4; cd->t_semicolon = $5; cd->classbody = $6; $$ = cd;}
       ;

Extension
       : /* empty */	              {$$ = nullptr;}		
       | EXTENDS IDENTIFIER		{struct Extension *e = new Extension(); e->token1 = $1; e->token2 = $2; $$ = e;}
       ;

ClassBody
       : LBRACE              RBRACE	{struct ClassBody *cb = new ClassBody(); cb->t_lbrace = $1; cb->t_rbrace = $2; $$ = cb;}
       | LBRACE ClassMembers RBRACE	{struct ClassBody *cb = new ClassBody(); cb->t_lbrace = $1; cb->classmembers = $2; cb->t_rbrace = $3; $$ = cb;}
       ;

ClassMembers
       :              ClassMember	 {struct ClassMembers *cm = new ClassMembers(); cm->classmember = $1; $$ = cm;}
       | ClassMembers ClassMember	 {struct ClassMembers *cm = new ClassMembers(); cm->classmembers = $1; cm->classmember = $2; $$ = cm;}
       ;

ClassMember
       : FieldDeclaration		{struct ClassMember *cm = new ClassMember(); cm->fielddeclaration = $1; $$ = cm;}
       | MethodDeclaration		{struct ClassMember *cm = new ClassMember(); cm->methoddeclaration = $1; $$ = cm;}
       ;

FieldDeclaration
       : Visibility Staticness Type IDENTIFIER SEMICOLON	{struct FieldDeclaration *fd = new FieldDeclaration(); fd->visibility = $1; fd->staticness = $2; fd->type = $3; fd->t_id = $4; fd->t_semicolon = $5; $$ = fd;}
       ;

Visibility
       : /* empty */	              {$$ = nullptr;}		
       | PRIVATE			{struct Visibility *v = new Visibility(); v->token = $1; $$ = v;}
       | PUBLIC			{struct Visibility *v = new Visibility(); v->token = $1; $$ = v;}
       ;

Staticness
       : /* empty */                      {$$ = nullptr;}
       | STATIC				{struct Staticness *s = new Staticness(); s->t_static = $1; $$ = s;}
       ;

MethodDeclaration
       : Visibility Staticness MethodType IDENTIFIER Parameters Body {struct MethodDeclaration *md = new MethodDeclaration(); md->visibility = $1; md->staticness = $2; md->methodtype = $3; md->t_id = $4; md->parameters = $5; md->body = $6; $$ = md;}
       ;

Parameters
       : LPAREN               RPAREN	{struct Parameters *ps = new Parameters(); ps->t_lparen = $1; ps->t_rparen = $2; $$ = ps;}
       | LPAREN ParameterList RPAREN	{struct Parameters *ps = new Parameters(); ps->t_lparen = $1; ps->parameterlist = $2; ps->t_rparen = $3; $$ = ps;}
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
       | LocalDeclarations LocalDeclaration	{struct LocalDeclarations *ld = new LocalDeclarations(); ld->localdeclarations = $1; ld->localdeclaration = $2; $$ = ld;}
       ;

LocalDeclaration
       : Type IDENTIFIER SEMICOLON		{struct LocalDeclaration *ld = new LocalDeclaration(); ld->type = $1; ld->t_id = $2; ld->t_semicolon = $3; $$ = ld;}
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
       : IF LPAREN Relation RPAREN Statement		       {struct IfStatement *is = new IfStatement(); is->t_if = $1; is->t_lparen = $2; is->relation = $3; is->t_rparen = $4; is->statement1 = $5; $$ = is;}
       | IF LPAREN Relation RPAREN Statement ELSE Statement	{struct IfStatement *is = new IfStatement(); is->t_if = $1; is->t_lparen = $2; is->relation = $3; is->t_rparen = $4; is->statement1 = $5; is->t_else = $6; is->statement2 = $7; $$ = is;}
       ;

WhileStatement
       : WHILE Relation LOOP Statement SEMICOLON	{struct WhileStatement *ws = new WhileStatement(); ws->t_while = $1; ws->relation = $2; ws->t_loop = $3; ws->statement = $4; ws->t_semicolon = $5; $$ = ws;}
       ;

ReturnStatement
       : RETURN            SEMICOLON	{struct ReturnStatement *rs = new ReturnStatement(); rs->t_return = $1; rs->t_semicolon = $2; $$ = rs;}
       | RETURN Expression SEMICOLON	{struct ReturnStatement *rs = new ReturnStatement(); rs->t_return = $1; rs->expression = $2; rs->t_semicolon = $3; $$ = rs;}
       ;

CallStatement
       : CompoundName LPAREN              RPAREN SEMICOLON	{struct CallStatement *cs = new CallStatement(); cs->compoundname = $1; cs->t_lparen = $2; cs->t_rparen = $3; cs->t_semicolon = $4; $$ = cs;}
       | CompoundName LPAREN ArgumentList RPAREN SEMICOLON	{struct CallStatement *cs = new CallStatement(); cs->compoundname = $1; cs->t_lparen = $2; cs->argumentlist = $3; cs->t_rparen = $4; cs->t_semicolon = $5; $$ = cs;}
       ;

ArgumentList
       :                    Expression	{struct ArgumentList *al = new ArgumentList(); al->expression = $1; $$ = al;}
       | ArgumentList COMMA Expression	{struct ArgumentList *al = new ArgumentList(); al->argumentlist = $1; al->t_comma = $2; al->expression = $3; $$ = al;}
       ;

PrintStatement
       : PRINT Expression SEMICOLON	{struct PrintStatement *ps = new PrintStatement(); ps->t_print = $1; ps->expression = $2; ps->t_semicolon = $3; $$ = ps;}
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
       | NEW NewType	                     		{struct Factor *f = new Factor(); f->t_new = $1; f->newtype = $2; $$ = f;}
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
       : /* empty */               {$$ = nullptr;}
       | LBRACKET RBRACKET		{struct ArrayTail *at = new ArrayTail(); at->t_lbracket = $1; at->t_rbracket = $2; $$ = at;}
       ;

%%

void compilationunit_traverse(CompilationUnit* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "CompilationUnit\n";
	spaces += SPACES_ADD;
	if (cur->imports) imports_traverse(cur->imports);
	if (cur->classdeclarations) classdeclarations_traverse(cur->classdeclarations);
	spaces -= SPACES_ADD;
}

void imports_traverse(Imports* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Imports\n";
	spaces += SPACES_ADD;
	if (cur->import) import_traverse(cur->import);
	if (cur->imports) imports_traverse(cur->imports);
	spaces -= SPACES_ADD;
}

void import_traverse(Import* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Import\n";
	spaces += SPACES_ADD;
	print_token(cur->token1);
	print_token(cur->token2);
	print_token(cur->token3);
	spaces -= SPACES_ADD;
}

void classdeclarations_traverse(ClassDeclarations* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "ClassDeclarations\n";
	spaces += SPACES_ADD;
	if (cur->classdeclaration) classdeclaration_traverse(cur->classdeclaration);
	if (cur->classdeclarations) classdeclarations_traverse(cur->classdeclarations);
	spaces -= SPACES_ADD;
}

void classdeclaration_traverse(ClassDeclaration* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "ClassDeclaration\n";
	spaces += SPACES_ADD;
	print_token(cur->t_public);
	print_token(cur->t_class);
	if (cur->compoundname) compoundname_traverse(cur->compoundname);
	if (cur->extension) extension_traverse(cur->extension);
	print_token(cur->t_semicolon);
	if (cur->classbody) classbody_traverse(cur->classbody);
	spaces -= SPACES_ADD;
}

void extension_traverse(Extension* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Extension\n";
	spaces += SPACES_ADD;
	print_token(cur->token1);
	print_token(cur->token2);
	spaces -= SPACES_ADD;
}

void classbody_traverse(ClassBody* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "ClassBody\n";
	spaces += SPACES_ADD;
	print_token(cur->t_lbrace);
	if (cur->classmembers) classmembers_traverse(cur->classmembers);
	print_token(cur->t_rbrace);
	spaces -= SPACES_ADD;
}

void classmembers_traverse(ClassMembers* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "ClassMembers\n";
	spaces += SPACES_ADD;
	if (cur->classmembers) classmembers_traverse(cur->classmembers);
	if (cur->classmember) classmember_traverse(cur->classmember);
	spaces -= SPACES_ADD;
}

void classmember_traverse(ClassMember* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "ClassMember\n";
	spaces += SPACES_ADD;
	if (cur->fielddeclaration) fielddeclaration_traverse(cur->fielddeclaration);
	if (cur->methoddeclaration) methoddeclaration_traverse(cur->methoddeclaration);
	spaces += SPACES_ADD;
}

void fielddeclaration_traverse(FieldDeclaration* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "FieldDeclaration\n";
	spaces += SPACES_ADD;
	if (cur->visibility) visibility_traverse(cur->visibility);
	if (cur->staticness) staticness_traverse(cur->staticness);
	if (cur->type) type_traverse(cur->type);
	print_token(cur->t_id);
	print_token(cur->t_semicolon);
		spaces -= SPACES_ADD;
}

void visibility_traverse(Visibility* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Visibility\n";
	spaces += SPACES_ADD;
	print_token(cur->token);
	spaces -= SPACES_ADD;
}

void staticness_traverse(Staticness* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Staticness\n";
	spaces += SPACES_ADD;
	print_token(cur->t_static);
	spaces -= SPACES_ADD;
}

void methoddeclaration_traverse(MethodDeclaration* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "MethodDeclaration\n";
	spaces += SPACES_ADD;
	if (cur->visibility) visibility_traverse(cur->visibility);
	if (cur->staticness) staticness_traverse(cur->staticness);
	if (cur->methodtype) methodtype_traverse(cur->methodtype);
	print_token(cur->t_id);
	if (cur->parameters) parameters_traverse(cur->parameters);
	if (cur->body) body_traverse(cur->body);
	spaces -= SPACES_ADD;
}

void parameters_traverse(Parameters* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Parameters\n";
	spaces += SPACES_ADD;
	print_token(cur->t_lparen);
	if (cur->parameterlist) parameterlist_traverse(cur->parameterlist);
	print_token(cur->t_rparen);
	spaces -= SPACES_ADD;
}

void parameterlist_traverse(ParameterList* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "ParameterList\n";
	spaces += SPACES_ADD;
	if (cur->parameterlist) parameterlist_traverse(cur->parameterlist);
	print_token(cur->t_comma);
	if (cur->parameter) parameter_traverse(cur->parameter);
	spaces -= SPACES_ADD;
}

void parameter_traverse(Parameter* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Parameter\n";
	spaces += SPACES_ADD;
	if (cur->type) type_traverse(cur->type);
	print_token(cur->t_id);
	spaces -= SPACES_ADD;
}

void methodtype_traverse(MethodType* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "MethodType\n";
	spaces += SPACES_ADD;
	if (cur->type) type_traverse(cur->type);
	print_token(cur->t_void);
	spaces -= SPACES_ADD;
}

void body_traverse(Body* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Body\n";
	spaces += SPACES_ADD;
	print_token(cur->t_lbrace);
	if (cur->localdeclarations) localdeclarations_traverse(cur->localdeclarations);
	if (cur->statements) statements_traverse(cur->statements);
	print_token(cur->t_rbrace);
	spaces -= SPACES_ADD;
}

void localdeclarations_traverse(LocalDeclarations* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "LocalDeclarations\n";
	spaces += SPACES_ADD;
	if (cur->localdeclarations) localdeclarations_traverse(cur->localdeclarations);
	if (cur->localdeclaration) localdeclaration_traverse(cur->localdeclaration);
	spaces -= SPACES_ADD;
}

void localdeclaration_traverse(LocalDeclaration* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "LocalDeclaration\n";
	spaces += SPACES_ADD;
	if (cur->type) type_traverse(cur->type);
	print_token(cur->t_id);
	print_token(cur->t_semicolon);
	spaces -= SPACES_ADD;
}

void statements_traverse(Statements* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Statements\n";
	spaces += SPACES_ADD;
	if (cur->statements) statements_traverse(cur->statements);
	if (cur->statement) statement_traverse(cur->statement);
	spaces -= SPACES_ADD;
}

void statement_traverse(Statement* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Statement\n";
	spaces += SPACES_ADD;
	if (cur->assignment) assignment_traverse(cur->assignment);
	if (cur->ifstatement) ifstatement_traverse(cur->ifstatement);
	if (cur->whilestatement) whilestatement_traverse(cur->whilestatement);
	if (cur->returnstatement) returnstatement_traverse(cur->returnstatement);
	if (cur->callstatement) callstatement_traverse(cur->callstatement);
	if (cur->printstatement) printstatement_traverse(cur->printstatement);
	if (cur->block) block_traverse(cur->block);
		spaces -= SPACES_ADD;
}

void assignment_traverse(Assignment* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Assignment\n";
	spaces += SPACES_ADD;
	if (cur->leftpart) leftpart_traverse(cur->leftpart);
	print_token(cur->t_assign);
	if (cur->expression) expression_traverse(cur->expression);
	print_token(cur->t_semicolon);
	spaces -= SPACES_ADD;
}

void ifstatement_traverse(IfStatement* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "IfStatement\n";
	spaces += SPACES_ADD;
	print_token(cur->t_if);
	print_token(cur->t_lparen);
	if (cur->relation) relation_traverse(cur->relation);
	print_token(cur->t_rparen);
	if (cur->statement1) statement_traverse(cur->statement1);
	print_token(cur->t_else);
	if (cur->statement2) statement_traverse(cur->statement2);
	spaces -= SPACES_ADD;
}

void whilestatement_traverse(WhileStatement* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "WhileStatement\n";
	spaces += SPACES_ADD;
	print_token(cur->t_while);
	if (cur->relation) relation_traverse(cur->relation);
	print_token(cur->t_loop);
	if (cur->statement) statement_traverse(cur->statement);
	print_token(cur->t_semicolon);
		spaces -= SPACES_ADD;
}

void returnstatement_traverse(ReturnStatement* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "ReturnStatement\n";
	spaces += SPACES_ADD;
	print_token(cur->t_return);
	if (cur->expression) expression_traverse(cur->expression);
	print_token(cur->t_semicolon);
	spaces -= SPACES_ADD;
}

void callstatement_traverse(CallStatement* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "CallStatement\n";
	spaces += SPACES_ADD;
	if (cur->compoundname) compoundname_traverse(cur->compoundname);
	print_token(cur->t_lparen);
	if (cur->argumentlist) argumentlist_traverse(cur->argumentlist);
	print_token(cur->t_rparen);
	print_token(cur->t_semicolon);
	spaces -= SPACES_ADD;
}

void argumentlist_traverse(ArgumentList* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "ArgumentList\n";
	spaces += SPACES_ADD;
	if (cur->argumentlist) argumentlist_traverse(cur->argumentlist);
	print_token(cur->t_comma);
	if (cur->expression) expression_traverse(cur->expression);
	spaces -= SPACES_ADD;
}

void block_traverse(Block* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Block\n";
	spaces += SPACES_ADD;
	print_token(cur->t_lbrace);
	if (cur->statements) statements_traverse(cur->statements);
	print_token(cur->t_rbrace);
	spaces -= SPACES_ADD;
}

void relation_traverse(Relation* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Relation\n";
	spaces += SPACES_ADD;
	if (cur->expression1) expression_traverse(cur->expression1);
	if (cur->relationaloperator) relationaloperator_traverse(cur->relationaloperator);
	if (cur->expression2) expression_traverse(cur->expression2);
		spaces -= SPACES_ADD;
}

void relationaloperator_traverse(RelationalOperator* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "RelationalOperator\n";
	spaces += SPACES_ADD;
	print_token(cur->token);
	spaces -= SPACES_ADD;
}  

void arraytail_traverse(ArrayTail* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "ArrayTail\n";
	spaces += SPACES_ADD;
	print_token(cur->t_lbracket);
	print_token(cur->t_rbracket);
	spaces -= SPACES_ADD;
}

void type_traverse(Type* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Type\n";
	spaces += SPACES_ADD;
	print_token(cur->token);
	if (cur->arraytail) arraytail_traverse(cur->arraytail);
	spaces -= SPACES_ADD;
}

void compoundname_traverse(CompoundName* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "CompoundName\n";
	spaces += SPACES_ADD;
	if (cur->compoundname) compoundname_traverse(cur->compoundname);
	print_token(cur->t_dot);
	print_token(cur->t_id);
	spaces -= SPACES_ADD;
}

void leftpart_traverse(LeftPart* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "LeftPart\n";
	spaces += SPACES_ADD;
	if (cur->compoundname) compoundname_traverse(cur->compoundname);
	print_token(cur->t_lbracket);
	if (cur->expression) expression_traverse(cur->expression);
	print_token(cur->t_rbracket);
	spaces -= SPACES_ADD;
}

void newtype_traverse(NewType* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "NewType\n";
	spaces += SPACES_ADD;
	print_token(cur->token);
	spaces -= SPACES_ADD;
}

void factor_traverse(Factor* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Factor\n";
	spaces += SPACES_ADD;
	print_token(cur->t_number);
	print_token(cur->t_null);
	if (cur->leftpart) leftpart_traverse(cur->leftpart);
	print_token(cur->t_new);
	if (cur->newtype) newtype_traverse(cur->newtype);
	print_token(cur->t_lbracket);
	if (cur->expression) expression_traverse(cur->expression);
	print_token(cur->t_rbracket);
	spaces -= SPACES_ADD;
}

void multsign_traverse(MultSign* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "MultSign\n";
	spaces += SPACES_ADD;
	print_token(cur->multsign);
	spaces -= SPACES_ADD;
}

void factors_traverse(Factors* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Factors\n";
	spaces += SPACES_ADD;
	if (cur->multsign) multsign_traverse(cur->multsign);
	if (cur->factor) factor_traverse(cur->factor);
	if (cur->factors) factors_traverse(cur->factors);
	spaces -= SPACES_ADD;
}

void term_traverse(Term* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Term\n";
	spaces += SPACES_ADD;
	if (cur->factor) factor_traverse(cur->factor);
	if (cur->factors) factors_traverse(cur->factors);
	spaces -= SPACES_ADD;
}

void addsign_traverse(AddSign* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "AddSign\n";
	spaces += SPACES_ADD;
	print_token(cur->addsign);
	spaces -= SPACES_ADD;
}

void terms_traverse(Terms* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Terms\n";
	spaces += SPACES_ADD;
	if (cur->addsign) addsign_traverse(cur->addsign);
	if (cur->term) term_traverse(cur->term);
	if (cur->terms) terms_traverse(cur->terms);
	spaces -= SPACES_ADD;
}

void expression_traverse(Expression* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "Expression\n";
	spaces += SPACES_ADD;
	if (cur->addsign) addsign_traverse(cur->addsign);
	if (cur->term) term_traverse(cur->term);
	if (cur->terms) terms_traverse(cur->terms);
	spaces -= SPACES_ADD;
}

void printstatement_traverse(PrintStatement* cur) {
	for (int i = 0; i < spaces; i++, cout << " "); cout << "| ";
	cout << "PrintStatement\n";
	spaces += SPACES_ADD;
	print_token(cur->t_print);
       if (cur->expression) expression_traverse(cur->expression);
	print_token(cur->t_semicolon);
	spaces -= SPACES_ADD;
}
