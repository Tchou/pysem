open Aliases

let of_module (env : Env.t) (m : PC.Module.t) : Ast.prog =
  (* TODO factor *)
  let sid = Ast.scoped_identifiers env env.module_ in
  (*Printing.dbg_pr "PROG"
    "nl_unused: @[%a@]@\n- nl_used: @[%a@]@\n- locals: @[%a@]%!"
    Ast.IdentSet.pp sid.nl_unused Ast.IdentSet.pp sid.nl_used
    Ast.IdentSet.pp sid.locals;*)
  List.map (Instr.of_statement env) m.body, sid.locals

let to_ml (prog,_ : Ast.prog) : (MlVar.t * ML.Ast.t) list =
  let ml = List.map Instr.to_ml prog in
  Builtins.all () @ ml
