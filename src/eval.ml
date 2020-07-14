open Syntax

type exval =
    IntV of int
  | BoolV of bool
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | And, BoolV i1, BoolV i2 -> BoolV (i1 && i2)
  | And, _, _ -> err ("Both arguments must be integer: &&")
  | Or, BoolV i1, BoolV i2 -> BoolV (i1 || i2)
  | Or, _, _ -> err ("Both arguments must be integer: ||")

let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
      Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in 
    (match op, arg1 with
      And, BoolV false -> BoolV false
      | Or, BoolV true -> BoolV true
      | _, _ -> let arg2 = eval_exp env exp2 in apply_prim op arg1 arg2)
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
      BoolV true -> eval_exp env exp2
      | BoolV false -> eval_exp env exp3
      | _ -> err ("Test expression must be boolean: if"))
  | LetInExp (id, exp1, exp2) ->
      let arg1 = eval_exp env exp1 in
      let newenv = Environment.extend id (arg1) env in
      eval_exp newenv exp2

let rec eval_letexp_env env = function
    UniLetExp (id, exp) -> 
      let v = eval_exp env exp in Environment.extend id v env
  | MulLetExp (id, exp1, exp2) ->
      let v = eval_exp env exp1 in eval_letexp_env (Environment.extend id v env) exp2


let eval_decl env = function
    Exp e -> let v = eval_exp env e in ("-", env, v)
  | LetExp e -> match e with
      UniLetExp (id, exp) ->
        let v = eval_exp env exp in (id, Environment.extend id v env, v)
    | MulLetExp (id, exp1, _) ->
        let v = eval_exp env exp1 in (id, eval_letexp_env env e, v)
