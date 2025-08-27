open Mlsem_lang
open Mlsem
let dummy_def = PAst.Definitions []
let translate env stmt =
  let open PyreAst.Concrete.Statement in
  match stmt with
    FunctionDef r ->
    let proto = Arguments.translate_arguments r.args in
    let () = Format.printf "Function %s: @[%a@]@\n"
    (PyreAst.Concrete.Identifier.to_string r.name) 
    Arguments.pp_proto proto in
    PAst.new_annot (env.Env.to_loc r.location), dummy_def
  | _ -> PAst.new_annot Common.Position.dummy, dummy_def