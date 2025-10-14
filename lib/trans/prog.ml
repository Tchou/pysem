let of_module (env : Env.t) (m : PyreAst.Concrete.Module.t) : Ast.prog =
  List.map (Instr.of_statement env) m.body
