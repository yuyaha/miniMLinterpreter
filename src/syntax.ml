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

let rec freevar_ty = function
    TyInt -> MySet.empty
  | TyBool -> MySet.empty
  | TyVar tv -> MySet.singleton tv
  | TyFun (t1, t2) -> MySet.union (freevar_ty t1) (freevar_ty t2)

let fresh_tyvar = 
  let counter = ref 0 in
  let body () =
    let v = !counter in
      counter := v + 1; v
  in body

let rec string_of_ty = function
    TyInt -> "int"
  | TyBool -> "bool"
  | TyVar tv -> 
    if (0 <= tv && tv <= 25) then ("'" ^ Char.escaped (Char.chr (tv + 97))) 
    else let tv' = tv mod 26 and idx = tv / 26 in ("'" ^ Char.escaped (Char.chr (tv' + 97)) ^ string_of_int idx)
  | TyFun (t1, t2) -> string_of_ty t1 ^ " -> " ^ string_of_ty t2

let pp_ty v = print_string (string_of_ty v)

type tysc = TyScheme of tyvar list * ty

let tysc_of_ty ty = TyScheme ([], ty)

let freevar_tysc tysc = 
  let TyScheme (tylist, ty) = tysc in
  let tvset = freevar_ty ty in
  let rec rmv alpha set = 
    match alpha with
        [] -> tvset
      | x :: rest -> rmv rest (MySet.remove x set) in
  rmv tylist tvset