open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref
  | DProcV of id * exp
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV _ -> "<fun>"
  | DProcV _ -> "<dfun>"

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | Rand, BoolV i1, BoolV i2 -> BoolV (i1 && i2)
  | Rand, _, _ -> err ("Both arguments must be integer: &&")
  | Ror, BoolV i1, BoolV i2 -> BoolV (i1 || i2)
  | Ror, _, _ -> err ("Both arguments must be integer: ||")

let rec vrfy_exp_id l = function
    LetInAndExp1 (id, _, exp, _) ->
      (List.mem id l) || (vrfy_exp_id (id :: l) exp)
  | LetInAndExp2 (id, _, exp) -> 
      (List.mem id l) || (vrfy_exp_id (id :: l) exp)
  | LetInAndExp3 (id, _) -> List.mem id l
  | MtlRecInExp1 (id, _, _, exp, _) ->
      (List.mem id l) || (vrfy_exp_id (id :: l) exp)
  | MtlRecInExp2 (id, _, _, exp) ->
      (List.mem id l) || (vrfy_exp_id (id :: l) exp)
  | MtlRecInExp3 (id, _, _) -> List.mem id l
  | _ -> false

let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
      Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in 
    (match op, arg1 with
      Rand, BoolV false -> BoolV false
      | Ror, BoolV true -> BoolV true
      | _, _ -> let arg2 = eval_exp env exp2 in apply_prim op arg1 arg2)
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
        BoolV true -> eval_exp env exp2
      | BoolV false -> eval_exp env exp3
      | _ -> err ("Test expression must be boolean: if"))
  | LetInExp (id, exp1, exp2) ->
      let arg = eval_exp env exp1 in
      let newenv = Environment.extend id arg env in
      eval_exp newenv exp2
  | LetInAndExp1 (id, exp1, exp2, exp3) ->
      let arg = eval_exp env exp1 in
      let extenv1 = eval_exp_env env exp2 in
      let extenv2 = Environment.extend id arg extenv1 in eval_exp extenv2 exp3
  | LetInFunExp1 (id, exp1, exp2) ->
      let arg = eval_exp env exp1 in
      let extenv = Environment.extend id arg env in eval_exp extenv exp2
  | LetInFunExp2 (id, exp) -> ProcV (id, exp, ref env)
  | LetInFunExp3 (id, exp) -> ProcV (id, exp, ref env)
  | DFunExp (id1, id2, exp1, exp2) ->
      let extenv = Environment.extend id1 (DProcV (id2, exp1)) env in eval_exp extenv exp2
  | FunExp (id, exp) -> ProcV (id, exp, ref env)
  | MulFunExp1 (id, exp) -> ProcV (id, exp, ref env)
  | MulFunExp2 (id, exp) -> ProcV (id, exp, ref env)
  | MulFunExp3 (id, exp) -> ProcV (id, exp, ref env)
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
      (match funval with
          ProcV (id, body, env') ->
            let newenv = Environment.extend id arg !env' in
            eval_exp newenv body
        | DProcV (id, body) -> 
            let newenv = Environment.extend id arg env in
            eval_exp newenv body
        | _ -> err ("Non-function value is applied"))
  | LetRecInExp (id, para, exp1, exp2) -> 
      let dummyenv = ref Environment.empty in
      let v = ProcV (para, exp1, dummyenv) in
      let newenv = Environment.extend id v env in
      dummyenv := newenv;
      eval_exp newenv exp2
  | MtlRecInExp1 (id, para, exp1, exp2, exp3) ->
      let dummyenv = ref Environment.empty in
      let v = ProcV (para, exp1, dummyenv) in
      let extenv1 = Environment.extend id v env in
      dummyenv := extenv1;
      let extenv2 = eval_exp_env extenv1 exp2 in
      dummyenv := extenv2;
      eval_exp extenv2 exp3
  | _ -> err ("") (* never execute *)
and eval_exp_env env = function
    LetInAndExp2 (id, exp1, exp2) -> 
      let arg = eval_exp env exp1 in
      let extenv = eval_exp_env env exp2 in Environment.extend id arg extenv
  | LetInAndExp3 (id, exp) -> 
      let arg = eval_exp env exp in Environment.extend id arg env
  | MtlRecInExp2 (id, para, exp1, exp2) ->
      let v = ProcV (para, exp1, ref env) in
      let extenv1 = Environment.extend id v env in
      ref env := extenv1;
      let extenv2 = eval_exp_env extenv1 exp2 in extenv2
  | MtlRecInExp3 (id, para, exp) ->
      let v = ProcV (para, exp, ref env) in
      let extenv = Environment.extend id v env in
      ref env := extenv; extenv
  | _ -> err ("") (* never execute *)

let rec vrfy_decl_id l = function
    MulLetExp2 (_, _, id, _, exp, _) ->
      (List.mem id l) || (vrfy_decl_id (id :: l) exp)
  | LetAndExp1 (id, _, exp) ->
      (List.mem id l) || (vrfy_decl_id (id :: l) exp)
  | LetAndExp2 (id, _, exp) ->
      (List.mem id l) || (vrfy_decl_id (id :: l) exp)
  | LetAndExp3 (id, _) -> List.mem id l
  | _ -> false

let rec eval_decl_env env = function
    LetExp (id, exp) -> 
      let v = eval_exp env exp in Environment.extend id v env
  | MulLetExp1 (id, exp1, exp2) ->
      let v = eval_exp env exp1 in 
      let extenv = Environment.extend id v env in eval_decl_env extenv exp2
  | MulLetExp2 (id1, exp1, id2, exp2, exp3, exp4) ->
      let v1 = eval_exp env exp1 in
      let extenv1 = Environment.extend id1 v1 env in
      let v2 = eval_exp extenv1 exp2 in
      let extenv2 = eval_decl_env extenv1 exp3 in 
      let extenv3 = Environment.extend id2 v2 extenv2 in
      eval_decl_env extenv3 exp4
  | MulLetExp3 (id, exp1, exp2) ->
      let v = eval_exp env exp1 in 
      let extenv = Environment.extend id v env in eval_decl_env extenv exp2
  | LetAndExp1 (id, exp1, exp2) ->
      let v = eval_exp env exp1 in 
      let extenv = eval_decl_env env exp2 in Environment.extend id v extenv
  | LetAndExp2 (id, exp1, exp2) ->
      let v = eval_exp env exp1 in
      let extenv = eval_decl_env env exp2 in Environment.extend id v extenv
  | LetAndExp3 (id, exp) ->
      let v = eval_exp env exp in Environment.extend id v env
  | LetFunExp (id, exp) -> 
      let v = eval_exp env exp in Environment.extend id v env

let rec vrfy_recdecl_id l = function
    MtlRecExp1 (id, _, _, exp) ->
      (List.mem id l) || (vrfy_recdecl_id (id :: l) exp)
  | MtlRecExp2 (id, _, _, exp) ->
      (List.mem id l) || (vrfy_recdecl_id (id :: l) exp)
  | MtlRecExp3 (id, _, _) -> (List.mem id l)
  | _ -> false

let rec eval_recdecl_env env = function
    MtlRecExp2 (id, para, exp1, exp2) ->
      let v = ProcV (para, exp1, ref env) in
      let extenv1 = Environment.extend id v env in
      ref env := extenv1;
      let extenv2 = eval_recdecl_env extenv1 exp2 in extenv2
  | MtlRecExp3 (id, para, exp) ->
      let v = ProcV (para, exp, ref env) in
      let extenv = Environment.extend id v env in
      ref env := extenv; extenv
  | _ -> err ("") (* never execute *)

let eval_decl env = function
    Exp e -> (match vrfy_exp_id [] e with
        true -> err ("Variable a is bound several times in this matching")
      | false -> let v = eval_exp env e in ("-", env, v))
  | Decl e -> (match vrfy_decl_id [] e with
        true -> err ("Variable a is bound several times in this matching")
      | false -> match e with
          LetExp (id, exp) ->
            let v = eval_exp env exp in (id, Environment.extend id v env, v)
        | MulLetExp1 (id, exp, _) ->
            let v = eval_exp env exp in (id, eval_decl_env env e, v)
        | MulLetExp2 (id, exp, _, _, _, _) ->
            let v = eval_exp env exp in (id, eval_decl_env env e, v)
        | MulLetExp3 (id, exp, _) -> 
            let v = eval_exp env exp in (id, eval_decl_env env e, v)
        | LetAndExp1 (id, exp, _) ->
            let v = eval_exp env exp in (id, eval_decl_env env e, v)
        | LetFunExp (id, exp) -> 
            let v = eval_exp env exp in (id, eval_decl_env env e, v)
        | _ -> err ("") (* never execute*))
  | RecDecl e -> (match vrfy_recdecl_id [] e with
        true -> err ("Variable a is bound several times in this matching")
      | false -> match e with
          LetRecExp (id, para, exp) ->
            let dummyenv = ref Environment.empty in
            let v = ProcV (para, exp, dummyenv) in
            let newenv = Environment.extend id v env in
            dummyenv := newenv;
            (id, newenv, v)
        | MtlRecExp1 (id, para, exp1, exp2) ->
            let dummyenv = ref Environment.empty in
            let v = ProcV (para, exp1, dummyenv) in
            let extenv1 = Environment.extend id v env in
            dummyenv := extenv1;
            let extenv2 = eval_recdecl_env extenv1 exp2 in
            dummyenv := extenv2;
            (id, extenv2, v)
        | _ -> err ("") (* never execute *))