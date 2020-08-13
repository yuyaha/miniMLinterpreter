open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = tysc Environment.t

type subst = (tyvar * ty) list

let rec subst_type subst = function
    TyVar tv -> (match subst with
        [] -> TyVar tv
      | (tv', ty) :: t ->
        if (tv = tv') then subst_type t ty else subst_type t (TyVar tv))
  | TyFun (t1, t2) -> (match subst with
        [] -> TyFun (t1, t2)
      | _ -> TyFun (subst_type subst t1, subst_type subst t2))
  | ty -> ty

let rec eqs_of_subst = function
    [] -> []
  | (tv, ty) :: rest -> (TyVar tv, ty) :: eqs_of_subst rest

let rec subst_eqs s = function
  [] -> []
| (t1, t2) :: rest -> (subst_type s t1, subst_type s t2) :: subst_eqs s rest

let rec unify = function
    [] -> []
  | (t1, t2) :: rest ->
    if (t1 = t2) then unify rest
    else match t1, t2 with
        TyFun (t3, t4), TyFun (t5, t6) ->
          unify ((t3, t5) :: ((t4, t6) :: rest))
      | TyVar tv, _ ->
          if (MySet.member tv (freevar_ty t2)) 
          then err ("The type variable " ^ string_of_ty t1 ^ " occurs inside " ^ string_of_ty t2)
          else (tv, t2) :: unify (subst_eqs [(tv, t2)] rest)
      | _, TyVar tv ->
          if (MySet.member tv (freevar_ty t1)) 
          then err ("The type variable " ^ string_of_ty t2 ^ " occurs inside " ^ string_of_ty t1)
          else (tv, t1) :: unify (subst_eqs [(tv, t1)] rest)
      | _ -> err ("This expression has type " ^ string_of_ty t1 ^ " but an expression was expected of type " ^ string_of_ty t2)

let rec freevar_tyenv tyenv = 
  Environment.fold_right (fun x -> fun y -> MySet.union (freevar_tysc x) y) tyenv MySet.empty

let closure ty tyenv subst =
  let fv_tyenv' = freevar_tyenv tyenv in
  let fv_tyenv = MySet.bigunion (MySet.map (fun id -> freevar_ty (subst_type subst (TyVar id))) fv_tyenv') in
  let ids = MySet.diff (freevar_ty ty) fv_tyenv in
  TyScheme (MySet.to_list ids, ty)

let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | Rand -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Ror -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

let rec ty_exp tyenv = function
    Var x ->
      (try
        let TyScheme (vars, ty) = Environment.lookup x tyenv in
        let s = List.map (fun id -> (id, TyVar (fresh_tyvar ()))) vars in ([], subst_type s ty)
      with
          Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (s3, ty3) = ty_exp tyenv exp3 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ [(ty1, TyBool); (ty2, ty3)] in
      let s4 = unify eqs in (s4, subst_type s4 ty2)
  | LetInExp (id, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let tysc = closure ty1 tyenv [] in
      let newtyenv = Environment.extend id tysc tyenv in
      let (s2, ty2) = ty_exp newtyenv exp2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 ty2)
  | LetInAndExp1 (id, exp1, exp2, exp3) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let tysc = closure ty1 tyenv [] in
      let (s2, newtyenv) = ty_exp_tyenv tyenv exp2 in
      let (s3, ty3) = ty_exp (Environment.extend id tysc newtyenv) exp3 in
      let eqs = (eqs_of_subst s1) @ s2 @ (eqs_of_subst s3)  in
      let s4 = unify eqs in (s4, subst_type s4 ty3)
  | LetInFunExp1 (id, exp1, exp2) -> 
      let domty = TyVar (fresh_tyvar ()) in
      let newtyenv = Environment.extend id (TyScheme ([], domty)) tyenv in
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp newtyenv exp2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(domty, ty1)] in
      let s3 = unify eqs in (s3, subst_type s3 ty2)
  | LetInFunExp2 (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s, ranty =
        ty_exp (Environment.extend id (TyScheme ([], domty)) tyenv) exp in (s, TyFun (subst_type s domty, ranty))
  | LetInFunExp3 (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s, ranty =
        ty_exp (Environment.extend id (TyScheme ([], domty)) tyenv) exp in (s, TyFun (subst_type s domty, ranty))
  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s, ranty =
        ty_exp (Environment.extend id (TyScheme ([], domty)) tyenv) exp in (s, TyFun (subst_type s domty, ranty))
  | MulFunExp1 (id, exp) ->
        let domty = TyVar (fresh_tyvar ()) in
        let s, ranty =
          ty_exp (Environment.extend id (TyScheme ([], domty)) tyenv) exp in (s, TyFun (subst_type s domty, ranty))
  | MulFunExp2 (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s, ranty =
        ty_exp (Environment.extend id (TyScheme ([], domty)) tyenv) exp in (s, TyFun (subst_type s domty, ranty))
  | MulFunExp3 (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s, ranty =
        ty_exp (Environment.extend id (TyScheme ([], domty)) tyenv) exp in (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) ->
      let domty = TyVar (fresh_tyvar ()) in
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(ty1, TyFun(ty2, domty))] in
      let s3 = unify eqs in (s3, subst_type s3 domty)
  | LetRecInExp (id1, id2, exp1, exp2) ->
      let domty1 = TyVar (fresh_tyvar ()) in
      let domty2 = TyVar (fresh_tyvar ()) in
      let newtyenv1' = Environment.extend id1 (TyScheme ([], (TyFun (domty1, domty2)))) tyenv in
      let newtyenv1 = Environment.extend id2 (TyScheme ([], domty1)) newtyenv1' in
      let (s1, ty1) = ty_exp newtyenv1 exp1 in
      let eqs' = (eqs_of_subst s1) @ [(ty1, domty2)] in
      let s1' = unify eqs' in
      let newtyenv2 = Environment.extend id1 (closure (TyFun (subst_type s1' domty1, ty1)) tyenv []) tyenv in
      let (s2, ty2) = ty_exp newtyenv2 exp2 in
      let eqs = eqs' @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 ty2)
  | MtlRecInExp1 (id1, id2, exp1, exp2, exp3) ->
      let domty1 = TyVar (fresh_tyvar ()) in
      let domty2 = TyVar (fresh_tyvar ()) in
      let newtyenv1' = Environment.extend id1 (TyScheme ([], (TyFun (domty1, domty2)))) tyenv in
      let newtyenv1 = Environment.extend id2 (TyScheme ([], domty1)) newtyenv1' in
      let (s1, ty1) = ty_exp newtyenv1 exp1 in
      let eqs' = (eqs_of_subst s1) @ [(ty1, domty2)] in
      let s1' = unify eqs' in
      let (s2, newtyenv2') = ty_exp_tyenv tyenv exp2 in
      let newtyenv2 = Environment.extend id1 (closure (TyFun (subst_type s1' domty1, ty1)) tyenv []) newtyenv2' in
      let (s3, ty3) = ty_exp newtyenv2 exp3 in
      let eqs = eqs' @ s2 @ (eqs_of_subst s3) in
      let s4 = unify eqs in (s4, subst_type s4 ty3)
  | _ -> err ("Not Implemented!")
and ty_exp_tyenv tyenv = function
    LetInAndExp2 (id, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let tysc = closure ty1 tyenv [] in
      let (s2, newtyenv) = ty_exp_tyenv tyenv exp2 in 
      ((eqs_of_subst s1) @ s2, Environment.extend id tysc newtyenv)
  | LetInAndExp3 (id, exp) ->
      let (s, ty) = ty_exp tyenv exp in
      let tysc = closure ty tyenv [] in (eqs_of_subst s, Environment.extend id tysc tyenv)
  | MtlRecInExp2 (id1, id2, exp1, exp2) -> 
      let domty1 = TyVar (fresh_tyvar ()) in
      let domty2 = TyVar (fresh_tyvar ()) in
      let newtyenv1' = Environment.extend id1 (TyScheme ([], (TyFun (domty1, domty2)))) tyenv in
      let newtyenv1 = Environment.extend id2 (TyScheme ([], domty1)) newtyenv1' in
      let (s1, ty1) = ty_exp newtyenv1 exp1 in
      let (s2, newtyenv2') = ty_exp_tyenv tyenv exp2 in 
      let newtyenv2 = Environment.extend id1 (closure (TyFun (subst_type s1 domty1, ty1)) tyenv []) newtyenv2' in
      ((eqs_of_subst s1) @ [(ty1, domty2)] @ s2, newtyenv2)
  | MtlRecInExp3 (id1, id2, exp) ->
      let domty1 = TyVar (fresh_tyvar ()) in
      let domty2 = TyVar (fresh_tyvar ()) in
      let newtyenv1' = Environment.extend id1 (TyScheme ([], (TyFun (domty1, domty2)))) tyenv in
      let newtyenv1 = Environment.extend id2 (TyScheme ([], domty1)) newtyenv1' in
      let (s1, ty1) = ty_exp newtyenv1 exp in 
      let newtyenv2 = Environment.extend id1 (closure (TyFun (subst_type s1 domty1, ty1)) tyenv []) newtyenv1 in
      (eqs_of_subst s1 @ [(ty1, domty2)], newtyenv2)
  | _ -> err ("") (* never execute *)

let rec ty_decl tyenv = function
    Exp e -> let (_, ty) = ty_exp tyenv e in (tyenv, ty)
  | Decl e -> (match e with
      LetExp (_, exp) -> let (_, ty) = ty_exp tyenv exp in (ty_decl_tyenv tyenv e , ty)
    | MulLetExp1 (_, exp, _) ->
        let (_, ty) = ty_exp tyenv exp in (ty_decl_tyenv tyenv e , ty)
    | MulLetExp2 (_, exp, _, _, _, _) ->
        let (_, ty) = ty_exp tyenv exp in (ty_decl_tyenv tyenv e , ty)
    | MulLetExp3 (_, exp, _) -> 
        let (_, ty) = ty_exp tyenv exp in (ty_decl_tyenv tyenv e , ty)
    | LetAndExp1 (_, exp, _) ->
        let (_, ty) = ty_exp tyenv exp in (ty_decl_tyenv tyenv e , ty)
    | LetFunExp (_, exp) -> 
        let (_, ty) = ty_exp tyenv exp in (ty_decl_tyenv tyenv e , ty)
    | _ -> err ("") (* never execute *))
  | RecDecl e -> (match e with
      LetRecExp (id, para, exp) ->
        let domty1 = TyVar (fresh_tyvar ()) in
        let domty2 = TyVar (fresh_tyvar ()) in
        let newtyenv1' = Environment.extend id (TyScheme ([], (TyFun (domty1, domty2)))) tyenv in
        let newtyenv1 = Environment.extend para (TyScheme ([], domty1)) newtyenv1' in
        let (s, ty) = ty_exp newtyenv1 exp in
        let eqs = (eqs_of_subst s) @ [(ty, domty2)] in
        let s' = unify eqs in (ty_recdecl_tyenv tyenv e, TyFun (subst_type s' domty1, ty))
    | MtlRecExp1 (id, para, exp1, _) ->
        let domty1 = TyVar (fresh_tyvar ()) in
        let domty2 = TyVar (fresh_tyvar ()) in
        let newtyenv1' = Environment.extend id (TyScheme ([], (TyFun (domty1, domty2)))) tyenv in
        let newtyenv1 = Environment.extend para (TyScheme ([], domty1)) newtyenv1' in
        let (s, ty) = ty_exp newtyenv1 exp1 in
        let eqs = (eqs_of_subst s) @ [(ty, domty2)] in
        let s' = unify eqs in (ty_recdecl_tyenv tyenv e, TyFun (subst_type s' domty1, ty))
    | _ -> err ("") (* never execute *))
and ty_decl_tyenv tyenv = function
    LetExp (id, exp) -> 
      let (_, ty) = ty_exp tyenv exp in
      let tysc = closure ty tyenv [] in Environment.extend id tysc tyenv
  | MulLetExp1 (id, exp1, exp2) ->
      let (_, ty) = ty_exp tyenv exp1 in 
      let tysc = closure ty tyenv [] in
      let exttyenv = Environment.extend id tysc tyenv in ty_decl_tyenv exttyenv exp2
  | MulLetExp2 (id1, exp1, id2, exp2, exp3, exp4) ->
      let (_, ty1) = ty_exp tyenv exp1 in
      let tysc1 = closure ty1 tyenv [] in
      let exttyenv1 = Environment.extend id1 tysc1 tyenv in
      let (_, ty2) = ty_exp exttyenv1 exp2 in
      let tysc2 = closure ty2 tyenv [] in
      let exttyenv2 = ty_decl_tyenv exttyenv1 exp3 in 
      let exttyenv3 = Environment.extend id2 tysc2 exttyenv2 in
      ty_decl_tyenv exttyenv3 exp4
  | MulLetExp3 (id, exp1, exp2) ->
      let (_, ty) = ty_exp tyenv exp1 in 
      let tysc = closure ty tyenv [] in
      let exttyenv = Environment.extend id tysc tyenv in ty_decl_tyenv exttyenv exp2
  | LetAndExp1 (id, exp1, exp2) ->
      let (_, ty) = ty_exp tyenv exp1 in 
      let tysc = closure ty tyenv [] in
      let exttyenv = ty_decl_tyenv tyenv exp2 in Environment.extend id tysc exttyenv
  | LetAndExp2 (id, exp1, exp2) ->
      let (_, ty) = ty_exp tyenv exp1 in
      let tysc = closure ty tyenv [] in
      let exttyenv = ty_decl_tyenv tyenv exp2 in Environment.extend id tysc exttyenv
  | LetAndExp3 (id, exp) ->
      let (_, ty) = ty_exp tyenv exp in
      let tysc = closure ty tyenv [] in Environment.extend id tysc tyenv
  | LetFunExp (id, exp) -> 
      let (_, ty) = ty_exp tyenv exp in
      let tysc = closure ty tyenv [] in Environment.extend id tysc tyenv
and ty_recdecl_tyenv tyenv = function
    LetRecExp (id, para, exp) ->
      let domty1 = TyVar (fresh_tyvar ()) in
      let domty2 = TyVar (fresh_tyvar ()) in
      let newtyenv1' = Environment.extend id (TyScheme ([], (TyFun (domty1, domty2)))) tyenv in
      let newtyenv1 = Environment.extend para (TyScheme ([], domty1)) newtyenv1' in
      let (s, ty) = ty_exp newtyenv1 exp in
      let eqs = (eqs_of_subst s) @ [(ty, domty2)] in
      let s' = unify eqs in
      let tysc = closure (TyFun (subst_type s' domty1, ty)) tyenv [] in Environment.extend id tysc tyenv
  | MtlRecExp1 (id, para, exp1, exp2) ->
      let domty1 = TyVar (fresh_tyvar ()) in
      let domty2 = TyVar (fresh_tyvar ()) in
      let newtyenv1' = Environment.extend id (TyScheme ([], (TyFun (domty1, domty2)))) tyenv in
      let newtyenv1 = Environment.extend para (TyScheme ([], domty1)) newtyenv1' in
      let (s, ty) = ty_exp newtyenv1 exp1 in
      let eqs = (eqs_of_subst s) @ [(ty, domty2)] in
      let s' = unify eqs in
      let newtyenv2 = ty_recdecl_tyenv tyenv exp2 in 
      Environment.extend id (closure (TyFun (subst_type s' domty1, ty)) tyenv []) newtyenv2
  | MtlRecExp2 (id, para, exp1, exp2) ->
      let domty1 = TyVar (fresh_tyvar ()) in
      let domty2 = TyVar (fresh_tyvar ()) in
      let newtyenv1' = Environment.extend id (TyScheme ([], (TyFun (domty1, domty2)))) tyenv in
      let newtyenv1 = Environment.extend para (TyScheme ([], domty1)) newtyenv1' in
      let (s, ty) = ty_exp newtyenv1 exp1 in
      let eqs = (eqs_of_subst s) @ [(ty, domty2)] in
      let s' = unify eqs in
      let newtyenv2 = ty_recdecl_tyenv tyenv exp2 in 
      Environment.extend id (closure (TyFun (subst_type s' domty1, ty)) tyenv []) newtyenv2
  | MtlRecExp3 (id, para, exp) ->
      let domty1 = TyVar (fresh_tyvar ()) in
      let domty2 = TyVar (fresh_tyvar ()) in
      let newtyenv1' = Environment.extend id (TyScheme ([], (TyFun (domty1, domty2)))) tyenv in
      let newtyenv1 = Environment.extend para (TyScheme ([], domty1)) newtyenv1' in
      let (s, ty) = ty_exp newtyenv1 exp in
      let eqs = (eqs_of_subst s) @ [(ty, domty2)] in
      let s' = unify eqs in
      Environment.extend id (closure (TyFun (subst_type s' domty1, ty)) tyenv []) tyenv