open Pysem
open Utils.Aliases
open Utils

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
     (* dbg_pr "pyre-parsed expression" "%a" *)
       (* Sexplib0.Sexp.pp_hum (PC.Module.sexp_of_t m); *)
     (* pr "block_infos" "%a" *)
       (* (pp_print_list ~pp_sep:pp_print_space Parsing.pp_block_info) bil; *)

     let env = Env.init bil to_loc in
     let p = Prog.of_module env m in
     pr "pysem ast" "%a" Ast.pp_prog p;

     let ml = Prog.to_ml p in
     pr "mlsem ast" "%a"
       (pp_print_list ~pp_sep:pp_print_newline Ast.pp_ml_top) ml;

     let ms_exprs = List.map (fun (v,t) -> v, ML.Transform.transform t) ml in
     let module MSC = MS.Checker in
     let module MTS = MT.TyScheme in
     (* MS.Config.infer_overload := false; *)
     let v_tys =
       try
         List.fold_left (fun (tsl,mce) (v,ast) ->
             let annot = MS.Reconstruction.infer mce
                           (MS.Refinement.refinement_envs mce ast) ast in
             let tvs, gty = MSC.typeof_def mce annot ast
                            |> MTS.norm_and_simpl
                            |> MTS.get in
             let ts = MTS.mk tvs MlGTy.(ub gty |> mk) in
             Utils.dbg_pr ("typing "^(Ast.Ident.var_show v))
               "@{<bold>ast@}: @[%a@]@\n@{<bold>tys@}: @[%a@]"
               Ast.MSAstPrinter.pp_t ast MTS.pp ts;
             ((v,ts)::tsl, MlMVar.add_to_env v ts mce) )
           ([], MC.Env.empty)
           ms_exprs
         |> fst |> List.rev (* keep definition order *)
       with
       | MSC.Untypeable err ->
          Format.printf "%s : %a@.%!" err.title
            (Format.pp_print_option pp_print_string) err.descr;
          raise (MSC.Untypeable err)
     in
     pr "all types" "%a"
       (pp_print_list ~pp_sep:pp_print_nothing
          Ast.pp_ml_tys)
       v_tys

let () =
  if Unix.isatty Unix.stdout then Colors.add_ansi_marking Format.std_formatter;
  let main = MT.PEnv.(sequential_handler empty main) in
  try
    if !Utils.debug
    then begin
        MT.Recording.start_recording ();
        main () |> fst;
        MT.Recording.stop_recording ();
        MT.Recording.(tally_calls () |> save_to_file "tally_calls");
      end
    else main () |> fst
  with
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
