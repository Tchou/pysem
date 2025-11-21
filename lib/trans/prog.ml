open Utils.Aliases

let of_module (env : Env.t) (m : PC.Module.t) : Ast.prog =
  List.map (Instr.of_statement env) m.body

let to_ml (prog : Ast.prog) : (MlVar.t * ML.Ast.t) list =
  List.map Instr.to_ml prog
