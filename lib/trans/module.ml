open Mlsem_lang.PAst

let translate (env : Env.t) (m : PyreAst.Concrete.Module.t) : parser_program =
List.map (Statement.translate env) m.body