%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token TRUE
%token FALSE
%token <string> Id
%token <int> Num
%token LET  
%token REC
%token EQ
%token IN
%token FUN
%token ARROW
%token IF
%token THEN
%token ELSE
%token PLUS
%token MINUS
%token MUL
%token DIV
%token LT
%token LE
%token NE
%token AND
%token OR
%token LPAREN
%token RPAREN
%token APP
%token LBRAC
%token RBRAC
%token SEMI
%token COLONCOLON
%token EOF

%nonassoc LET FUN IF
%left AND OR 
%left EQ LT LE NE 
%right COLONCOLON SEMI LBRAC
%left PLUS MINUS 
%left MUL DIV 
%left APP
%start exp 
%type <Nano.expr> exp

%%

exp: | LET Id EQ exp IN exp    { Let($2, $4, $6) }
     | LET REC Id EQ exp IN exp { Letrec($3, $5, $7) }
     | FUN Id ARROW exp          { Fun($2, $4) }     
     | IF exp THEN exp ELSE exp { If($2, $4, $6) }

     | exp AND exp             { Bin($1, And, $3) }
     | exp OR exp              { Bin($1, Or, $3) }
     
     | exp EQ exp              { Bin($1, Eq, $3) }
     | exp NE exp              { Bin($1, Ne, $3) }
     | exp LT exp              { Bin($1, Lt, $3) }
     | exp LE exp              { Bin($1, Le, $3) }
     
     | exp COLONCOLON exp      { Bin($1, Cons, $3)}
     | exp SEMI exp            { Bin($1, Cons, $3)} 
     | LBRAC exp               { $2}
     | exp RBRAC               { Bin($1, Cons, NilExpr) }
     | opexp                   { $1 }

   opexp: 
     | exp PLUS exp            { Bin($1, Plus, $3) }
     | exp MINUS exp           { Bin($1, Minus, $3) }
     | opexp1                       { $1 }
     
  opexp1: 
     | opexp1 MUL exp1             { Bin($1, Mul, $3) }
     | opexp1 DIV exp1            { Bin($1, Div, $3) }
     | exp1                       { $1 }
   
  exp1: 
     | exp1 exp2                 { App($1, $2)} 
     | exp2                       { $1 }
 
  exp2:
     | Num                     { Const $1 }
     | Id                      { Var($1) }
     | TRUE                    { True }
     | FALSE                   { False }
     | LBRAC RBRAC             { NilExpr }
     | LPAREN exp RPAREN       { $2 }

