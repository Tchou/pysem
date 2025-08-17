open Pysem
let usage_message = Format.sprintf "%s <file.py>" Sys.argv.(0)
let input_file = ref None

let add_input_file s = 
  match !input_file with 
    None -> input_file := (Some s)
  | Some _ -> raise (Arg.Bad "multiple files provided")

let options = Arg.align []


let main () =
  Arg.parse options add_input_file usage_message;
  match !input_file with
    None ->
    Format.eprintf "%s: missing file@\n%s" Sys.argv.(0) 
      (Arg.usage_string options usage_message)
  | Some file ->
    let stmts,_,vars, bil = Parsing.parse_partial ~file in
    let open Format in
    List.iter (function 
        | Ok s ->
          printf "%a@\n--@\n"  Sexplib0.Sexp.pp_hum (PyreAst.Concrete.Statement.sexp_of_t s)
        | Error e -> 
          let open PyreAst.Parser.Error in
          printf "%d:%d-%d:%d : %s@\n--@\n" 
            e.line e.column e.end_line e.end_column e.message
      ) stmts;
    printf "@[<v>%a@]@\n--@\n" Parsing.pp_vars (Parsing.IdentMap.bindings vars);
    printf "%a@\n"
      (pp_print_list ~pp_sep:pp_print_space Parsing.pp_block_info) bil


let () = try main () with
  | Parsing.Syntax (file, e) -> 
    Format.eprintf "%s: %d:%d-%d:%d : %s@\n" 
      file e.line e.column e.end_line e.end_column e.message;
    exit 3 
  | Sys_error msg -> Format.eprintf "%s@\n" msg; exit 1
  | e  -> Format.eprintf "ERROR: %s@\n" (Printexc.to_string e); exit 10
