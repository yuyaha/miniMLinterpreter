%{
open Syntax
%}

%token EQUAL LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE LET IN

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | e=LetExpr SEMISEMI { LetExp e }

Expr :
    e=LetInExpr { e }
  | e=IfExpr { e }
  | e=ORExpr { e }

ORExpr :
    l=ORExpr OR r=ANDExpr { BinOp (Or, l, r) }
  | e=ANDExpr { e }

ANDExpr :
    l=ANDExpr AND r=LTExpr { BinOp (And, l, r) }
  | e=LTExpr { e }

LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    l=MExpr MULT r=AExpr { BinOp (Mult, l, r) }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetInExpr :
    LET i=ID EQUAL s=Expr IN e=Expr { LetInExp (i, s, e) }

LetExpr :
    LET i=ID EQUAL s=Expr { UniLetExp (i, s) }
  | LET i=ID EQUAL s=Expr e=LetExpr { MulLetExp (i, s, e) }
