%{
  open Interpreter
%}

%token TRUE FALSE
%token <string> VAR CONST
%token <int> NUMBER
%token FAIL
%token SEP
%token OPEN_PAREN
%token CLOS_PAREN
%token OPEN_SQ
%token CLOS_SQ
%token LIST_DIFF
%token CUT
%token EOF
%token EQUALS GREATER LESS
%token ADD SUB MUL DIV EXP
%token COMMA SEM_COL END

%left COMMA
%left SEM_COL
%nonassoc EQUALS GREATER LESS
%left ADD SUB
%left MUL DIV
%left EXP
%nonassoc END

%start database
%start query

%type <Interpreter.program> database
%type <Interpreter.goal> query
%%

database: 
	|EOF			 				{ [] }
	|p_clause database				{ ($1)::($2) }
;

query:
	|END								{ [] }
	|p_atomlist query						{ ($1)@($2) }
;

p_clause:
	|p_atom END						{ Fact($1) }
	|p_atom SEP p_atomlist END		{ Rule($1, $3) }
;

p_atom:
	|CONST OPEN_PAREN p_termlist CLOS_PAREN	{ Pred ($1,$3) }
	|CUT									{ Cut }
	|FAIL									{ Fail }
;

p_atomlist:
	|p_atom 								{ [$1] }
	|p_atom COMMA p_atomlist 				{ ($1)::($3) }
;

p_term:
	|VAR					{ Var($1) }
	|CONST					{ Const($1) }
	|NUMBER					{ Nat($1) }
	|TRUE					{ T }
	|FALSE					{ F }
	|p_term EQUALS p_term 	{ Node ("Equals",[$1;$3]) }
	|p_term GREATER p_term 	{ Node ("Greater",[$1;$3]) }
	|p_term LESS p_term 	{ Node ("Less",[$1;$3]) }
	|p_term ADD p_term 		{ Node ("Add",[$1;$3]) }
	|p_term SUB p_term 		{ Node ("Sub",[$1;$3]) }
	|p_term MUL p_term 		{ Node ("Mul",[$1;$3]) }
	|p_term DIV p_term 		{ Node ("Div",[$1;$3]) }
	|p_term EXP p_term 		{ Node ("Exp",[$1;$3]) }
	|OPEN_PAREN p_term CLOS_PAREN { $2 }
	|OPEN_SQ p_list CLOS_SQ { $2 }
	|OPEN_SQ CLOS_SQ 		{Node("Cons",[])}
;

p_termlist:
	|p_term 					{ [$1] }
	|p_term COMMA p_termlist	{ ($1)::($3) }
;

p_list:
	|p_term 				{ Node ("Cons",[$1;Node ("Cons",[])]) }
	|p_term COMMA p_list	{ Node ("Cons",[($1);($3)]) }
	|p_term LIST_DIFF VAR	{ Node ("Cons",[($1);Var($3)]) }
;