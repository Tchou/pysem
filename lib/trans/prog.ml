open Utils.Aliases

let of_module (env : Env.t) (m : PC.Module.t) : Ast.prog =
  List.map (Instr.of_statement env) m.body

let to_ml (prog : Ast.prog) : (MlVar.t * ML.Ast.t) list =
  let ml = List.map Instr.to_ml prog in
  (Hashtbl.fold (fun _ vt l -> vt::l) Instr.gen_builtins []) @ ml
