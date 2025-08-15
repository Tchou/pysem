
let parse ~file =
  let str = In_channel.(with_open_bin file input_all) in
  let open PyreAst in
  let pyast =
    Parser.with_context (fun context ->
        Parser.
          Concrete.parse_module ~context ~enable_type_comment:true str
      )
  in
  match pyast with
    Error e -> Error.syntax file e
  | Ok ast -> (Concrete.Module.sexp_of_t ast)
              |> Format.printf "%a\n" Sexplib0.Sexp.pp