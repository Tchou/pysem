open Aliases

let of_module (env : Env.t) (m : PC.Module.t) : Ast.prog =
  let globals, _ = Ast.used_identifiers env env.module_id in
  globals,
  List.map (Instr.of_statement env) m.body
let to_ml (prog : Ast.prog) : (MlVar.t * ML.Ast.t) list =
  let ml = List.map Instr.to_ml (snd prog) in
  Builtins.all () @ ml
