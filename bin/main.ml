open Pysem
let usage_message = Format.sprintf "%s <file.py>" Sys.argv.(0)
let input_file = ref None

let add_input_file s = 
  match !input_file with 
  | None -> input_file := (Some s)
  | Some _ -> raise (Arg.Bad "multiple files provided")

let options = Arg.align []

let main () =
  Arg.parse options add_input_file usage_message;
  match !input_file with
  | None ->
     Format.eprintf "%s: missing file@\n%s" Sys.argv.(0)
       (Arg.usage_string options usage_message)
  | Some file ->
     let m, bil, to_loc = Parsing.parse ~file in
     let open Format in
     printf "%a@\n--@\n"
       Sexplib0.Sexp.pp_hum (PyreAst.Concrete.Module.sexp_of_t m);
     printf "%a@\n"
       (pp_print_list ~pp_sep:pp_print_space Parsing.pp_block_info) bil;
     let env = Env.init bil to_loc in
     let p = Prog.of_module env m in
     Format.printf "pysem ast:@.%a@.--@\n" Ast.pp_prog p;
     let ml = Prog.to_ml p in
     Format.(printf "mlsem ast:@\n%a@."
               (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
                  Ast.MlAstPrinter.pp_t)) ml;
     Format.printf "---@.%!";
     let mlsys = List.map Mlsem_lang.Transform.transform ml in
     let module MSC  = Mlsem.System.Checker in
     let module MSRc = Mlsem.System.Reconstruction in
     let module MSRf = Mlsem.System.Refinement in
     let module MCE  = Mlsem.Common.Env in
     let module MCRE = Mlsem.Common.REnvSet in
     let typed =
       try
         List.map (fun ast ->
             Format.printf ">>@\n%a@." Mlsem.System.Ast.pp ast;
             MSC.typeof MCE.empty
               MSRc.(infer MCE.empty (MSRf.refinement_envs MCE.empty ast) ast)
               ast) mlsys
       with
       | MSC.Untypeable err ->
          Format.printf "%s : %a@.%!" err.title
            (Format.pp_print_option pp_print_string) err.descr;
          raise (MSC.Untypeable err)
     in
     List.iter (Format.printf "%a@." Mlsem.Types.GTy.pp) typed

let () =
  try main () with
  | Parsing.Syntax (file, e) -> 
     Format.eprintf "%s: %d:%d-%d:%d : %s@\n"
       file e.line e.column e.end_line e.end_column e.message;
     exit 3
  | Sys_error msg -> Format.eprintf "%s@\n" msg; exit 1
  | e  -> 
     Format.eprintf "ERROR: %s@\n%s@\n"
       (Printexc.to_string e)
       (Printexc.get_backtrace ());
     exit 10
