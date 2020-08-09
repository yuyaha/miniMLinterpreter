%{
open Syntax
%}

%token EQUAL LPAREN RPAREN SEMISEMI
%token PLUS MULT LT RAND ROR
%token IF THEN ELSE TRUE FALSE LET IN AND
%token RARROW FUN DFUN
%token REC

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | e=LetExpr SEMISEMI { Decl e }
  | e=LetRecExpr SEMISEMI { RecDecl e }

Expr :
    e=FunExpr { e }
  | e=LetInExpr { e }
  | e=IfExpr { e }
  | e=ORExpr { e }

ORExpr :
    l=ORExpr ROR r=ANDExpr { BinOp (Ror, l, r) }
  | e=ANDExpr { e }

ANDExpr :
    l=ANDExpr RAND r=LTExpr { BinOp (Rand, l, r) }
  | e=LTExpr { e }

LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }

AppExpr :
    e1=AppExpr e2=AExpr { AppExp(e1, e2) }
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
  | LET i=ID EQUAL s=Expr e1=LetInAndExpr IN e2=Expr { LetInAndExp1 (i, s, e1, e2) }
  | LET i=ID e1=LetFunExpr IN e2=Expr { LetInFunExp1 (i, e1, e2) }
  | LET i1=ID EQUAL DFUN i2=ID RARROW e1=Expr IN e2=Expr { DFunExp (i1, i2, e1, e2) }
  | LET REC i=ID EQUAL FUN p=ID RARROW e1=Expr IN e2=Expr { LetRecInExp (i, p, e1, e2) }
  | LET REC i=ID EQUAL FUN p=ID RARROW e1=Expr e2=MtlRecInExpr IN e3=Expr { MtlRecInExp1 (i, p, e1, e2, e3) }

LetInAndExpr :
    AND i=ID EQUAL e1=Expr e2=LetInAndExpr { LetInAndExp2 (i, e1, e2) }
  | AND i=ID EQUAL e=Expr { LetInAndExp3 (i, e) }

LetFunExpr :
    i=ID e=LetFunExpr { LetInFunExp2 (i, e) }
  | i=ID EQUAL e=Expr { LetInFunExp3 (i, e) }

MtlRecInExpr :
    AND i=ID EQUAL FUN p=ID RARROW e1=Expr e2=MtlRecInExpr { MtlRecInExp2 (i, p, e1, e2) }
  | AND i=ID EQUAL FUN p=ID RARROW e=Expr { MtlRecInExp3 (i, p, e) }

FunExpr :
    FUN i=ID RARROW e=Expr { FunExp (i, e) }
  | FUN i=ID e=MulFunExpr { MulFunExp1 (i, e) }

MulFunExpr :
    i=ID e=MulFunExpr { MulFunExp2 (i, e) }
  | i=ID RARROW e=Expr { MulFunExp3 (i, e) }

LetExpr :
    LET i=ID EQUAL e=Expr { LetExp (i, e) }
  | LET i=ID EQUAL e1=Expr e2=LetAndExpr { LetAndExp1 (i, e1, e2) }
  | LET i=ID e=LetFunExpr { LetFunExp (i, e) }
  | LET i=ID EQUAL e1=Expr e2=LetExpr { MulLetExp1 (i, e1, e2) }
  | LET i1=ID EQUAL e1=Expr LET i2=ID EQUAL e2=Expr e3=LetAndExpr e4=LetExpr { MulLetExp2 (i1, e1, i2, e2, e3, e4) }
  | LET i=ID e1=LetFunExpr e2=LetExpr { MulLetExp3 (i, e1, e2) }

LetAndExpr :
    AND i=ID EQUAL e1=Expr AND e2=LetAndExpr { LetAndExp2 (i, e1, e2) }
  | AND i=ID EQUAL e=Expr { LetAndExp3 (i, e) }

LetRecExpr :
    LET REC i=ID EQUAL FUN p=ID RARROW e=Expr { LetRecExp (i, p, e) }
  | LET REC i=ID EQUAL FUN p=ID RARROW e1=Expr e2=MtlRecExpr { MtlRecExp1 (i, p, e1, e2) }

MtlRecExpr :
    AND i=ID EQUAL FUN p=ID RARROW e1=Expr e2=MtlRecExpr { MtlRecExp2 (i, p, e1, e2) }
  | AND i=ID EQUAL FUN p=ID RARROW e=Expr { MtlRecExp3 (i, p, e) }
