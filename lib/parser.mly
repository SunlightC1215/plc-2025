%{
open Ast
%}

%token <string> ID
%token <int> INT_LIT
%token INT VOID IF ELSE WHILE BREAK CONTINUE RETURN
%token OR AND EQ NEQ LT GT LE GE ASSIGN 
%token PLUS MINUS TIMES DIV MOD NOT
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA
%token EOF

%start main
%type <Ast.program> main

%%

main:
  | funcs EOF { $1 }

funcs:
  | func { [$1] }
  | func funcs { $1 :: $2 }

func:
  | INT ID LPAREN params_opt RPAREN block { 
      { name=$2; ret_ty=`Int; params=$4; body=$6 } }
  | VOID ID LPAREN params_opt RPAREN block {
      { name=$2; ret_ty=`Void; params=$4; body=$6 } }

params_opt:
  | /* empty */ { [] }
  | params { $1 }
params:
  | INT ID { [$2] }
  | INT ID COMMA params { $2 :: $4 }

block:
  | LBRACE stmts RBRACE { $2 }

stmts:
  | /* empty */ { [] }
  | stmt stmts { $1 :: $2 }

stmt:
  | block { Block $1 }
  | SEMI { Empty }
  | expr SEMI { Expr $1 }
  | ID ASSIGN expr SEMI { Assign($1,$3) }
  | INT ID ASSIGN expr SEMI { Decl($2,$4) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3,$5,Some $7) }
  | IF LPAREN expr RPAREN stmt { If($3,$5,None) }
  | WHILE LPAREN expr RPAREN stmt { While($3,$5) }
  | BREAK SEMI { Break }
  | CONTINUE SEMI { Continue }
  | RETURN expr SEMI { Return(Some $2) }
  | RETURN SEMI { Return(None) }

expr:
  | expr OR expr1 { BinOp(Or, $1, $3) }
  | expr1 { $1 }

expr1:
  | expr1 AND expr2 { BinOp(And, $1, $3) }
  | expr2 { $1 }

expr2:
  | expr2 EQ expr3 { BinOp(Eq, $1, $3) }
  | expr2 NEQ expr3 { BinOp(Neq, $1, $3) }
  | expr2 LT expr3 { BinOp(Lt, $1, $3) }
  | expr2 GT expr3 { BinOp(Gt, $3, $1) }
  | expr2 LE expr3 { BinOp(Le, $3, $1) }
  | expr2 GE expr3 { BinOp(Ge, $1, $3) }
  | expr3 { $1 }

expr3:
  | expr3 PLUS expr4 { BinOp(Add, $1, $3) }
  | expr3 MINUS expr4 { BinOp(Sub, $1, $3) }
  | expr4 { $1 }

expr4:
  | expr4 TIMES expr5 { BinOp(Mul, $1, $3) }
  | expr4 DIV expr5 { BinOp(Div, $1, $3) }
  | expr4 MOD expr5 { BinOp(Mod, $1, $3) }
  | expr5 { $1 }

expr5:
  | MINUS expr5 { UnOp(Neg, $2) }
  | PLUS expr5 { UnOp(Pos, $2) }
  | NOT expr5 { UnOp(Not, $2) }
  | atom { $1 }

atom:
  | ID { Var $1 }
  | INT_LIT { Int $1 }
  | ID LPAREN args_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

args_opt:
  | /* empty */ { [] }
  | args { $1 }
args:
  | expr { [$1] }
  | expr COMMA args { $1 :: $3 }