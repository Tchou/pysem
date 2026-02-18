open Aliases

let of_module (env : Env.t) (m : PC.Module.t) : Ast.prog * Ast.IdentSet.t =
  (* TODO factor *)
  let mod_bid = Parsing.BlockId.mk_module env.Env.current.filename Parsing.dummy_loc in
  let ids = Ast.used_identifiers env mod_bid in

  List.map (Instr.of_statement env) m.body, ids

let to_ml (prog : Ast.prog) : (MlVar.t * ML.Ast.t) list =
  let ml = List.map Instr.to_ml prog in
  Builtins.all () @ ml
