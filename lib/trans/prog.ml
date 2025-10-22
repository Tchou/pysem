let of_module (env : Env.t) (m : PyreAst.Concrete.Module.t) : Ast.prog =
  List.map (Instr.of_statement env) m.body

let to_ml (env : Env.t) (prog : Ast.prog) : Mlsem_lang.Ast.t list =
  List.map (Instr.to_ml env) prog
