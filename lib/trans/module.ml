let translate env mod_ast =
  let open PyreAst.Concrete.Module in
  List.fold_left (fun (env, l) stmt -> 
      let env', stmt' = Statement.toplevel env stmt in
      env', stmt' :: l
    ) (env, []) mod_ast.body
  |> snd |> List.rev
