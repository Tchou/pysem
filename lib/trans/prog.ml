open Aliases

let of_module (env : Env.t) (m : PC.Module.t) : Ast.prog =
  (* TODO factor *)
  let mod_bid = Parsing.BlockId.mk_module env.Env.current.filename Parsing.dummy_loc in
  let sid = Ast.scoped_identifiers env mod_bid in
  Format.printf "PROG:\n- nl_unused: %a\n- nl_used:   %a\n- locals:    %a\n%!"
    Ast.IdentSet.pp sid.nl_unused Ast.IdentSet.pp sid.nl_used
    Ast.IdentSet.pp sid.locals;
  List.map (Instr.of_statement env) m.body, sid.locals

let to_ml (prog,_ : Ast.prog) : (MlVar.t * ML.Ast.t) list =
  let ml = List.map Instr.to_ml prog in
  Builtins.all () @ ml
