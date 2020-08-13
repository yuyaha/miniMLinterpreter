open Eval
open Typing
open Syntax

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (newtyenv, ty) = ty_decl tyenv decl in
    let (id, newenv, v) = eval_decl env decl in
    Printf.printf "val %s : " id;
    pp_ty ty;
    print_string " = ";
    pp_val v;
    print_newline();
    read_eval_print newenv newtyenv
  with
    Error err -> 
      print_string err;
      print_newline();
      read_eval_print env tyenv
    | Failure fai ->
      print_string fai;
      print_newline();
      read_eval_print env tyenv

let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "ii" (IntV 2)
      (Environment.extend "iii" (IntV 3)
        (Environment.extend "iv" (IntV 4)
          (Environment.extend "v" (IntV 5)
            (Environment.extend "x" (IntV 10) Environment.empty)))))

let initial_tyenv =
  Environment.extend "i" (TyScheme ([], TyInt))
    (Environment.extend "ii" (TyScheme ([], TyInt))
      (Environment.extend "iii" (TyScheme ([], TyInt))
        (Environment.extend "iv" (TyScheme ([], TyInt))
          (Environment.extend "v" (TyScheme ([], TyInt))
            (Environment.extend "x" (TyScheme ([], TyInt)) Environment.empty)))))