(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | Rand | Ror

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetInExp of id * exp * exp
  | LetInAndExp1 of id * exp * exp * exp
  | LetInAndExp2 of id * exp * exp
  | LetInAndExp3 of id * exp
  | LetInFunExp1 of id * exp * exp
  | LetInFunExp2 of id * exp
  | LetInFunExp3 of id * exp
  | DFunExp of id * id * exp * exp
  | FunExp of id * exp
  | MulFunExp1 of id * exp
  | MulFunExp2 of id * exp
  | MulFunExp3 of id * exp
  | AppExp of exp * exp
  | LetRecInExp of id * id * exp * exp
  | MtlRecInExp1 of id * id * exp * exp * exp
  | MtlRecInExp2 of id * id * exp * exp
  | MtlRecInExp3 of id * id * exp

type letexp = 
    LetExp of id * exp
  | MulLetExp1 of id * exp * letexp
  | MulLetExp2 of id * exp * id * exp * letexp * letexp
  | MulLetExp3 of id * exp * letexp
  | LetAndExp1 of id * exp * letexp
  | LetAndExp2 of id * exp * letexp
  | LetAndExp3 of id * exp
  | LetFunExp of id * exp

type letrecexp =
    LetRecExp of id * id * exp
  | MtlRecExp1 of id * id * exp * letrecexp
  | MtlRecExp2 of id * id * exp * letrecexp
  | MtlRecExp3 of id * id * exp

type program =
    Exp of exp
  | Decl of letexp
  | RecDecl of letrecexp

type tyvar = int
type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty

let freevar_ty _ =
  assert false (* Exercise 4.3.1 *)

let string_of_ty _ =
  assert false (* Exercise 4.3.1 *)

let pp_ty ty =
  print_string (string_of_ty ty)
