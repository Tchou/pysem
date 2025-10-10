open Mlsem_app.PAst

let translate (env : Env.t) (m : PyreAst.Concrete.Module.t) : parser_program =
  List.map (Statement.translate_top env) m.body
