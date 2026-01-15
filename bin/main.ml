open Pysem
open Aliases
open Printing

module MSC = MS.Checker
module MTS = MT.TyScheme

let usage_message = Format.sprintf "%s <file.py>" Sys.argv.(0)
let input_file = ref None

let shadowing = ref false
(** Allow shadowing in toplevel. **)

let add_input_file s =
  match !input_file with
  | None -> input_file := (Some s)
  | Some _ -> raise (Arg.Bad "multiple files provided")

let options = Arg.align []

let ml_type mcenv ast =
  let annot = MS.Reconstruction.infer mcenv
      (MS.Refinement.refinement_envs mcenv ast) ast in
  MSC.typeof_def mcenv annot ast
  |> MTS.norm_and_simpl

let upd_env mce v ts =
  ( if MC.Env.mem v mce
    then
      if !shadowing
      then MC.Env.rm v mce
      else failwith (Format.sprintf "Cannot add '%s' twice to environement!"
                       (mlvar_show v))
    else mce )
  |> MC.Env.add v ts

let ms_of_us t = t *. 1000.

let treat_def (tt, mce, lst) (v, ast) =
  let time0 = Unix.gettimeofday () in
  let ts = ml_type mce ast in
  let v_str = MlVar.show v in
  let time1 = Unix.gettimeofday () in

  let elapsed = time1 -. time0 in
  dbg_pr ("typing "^ v_str)
    "@{<italic;yellow>%.2fms@}@\n@{<bold;blue>ast@}: @[%a@]@\n\
     @{<bold;blue>tys@}: @[%a@]"
    (ms_of_us elapsed)
    MSAstPrinter.pp_t ast MTS.pp ts;
  tt +. elapsed, upd_env mce v ts, v::lst

let main () =
  Arg.parse options add_input_file usage_message;
  match !input_file with
  | None ->
    Format.eprintf "%s: missing file@\n%s" Sys.argv.(0)
      (Arg.usage_string options usage_message)
  | Some file ->
    let open Format in
    let m, bil, to_loc = Parsing.parse ~file in
    (* dbg_pr "pyre-parsed expression" "%a" *)
    (* Sexplib0.Sexp.pp_hum (PC.Module.sexp_of_t m); *)
    (* pr "block_infos" "%a" *)
    (* (pp_print_list ~pp_sep:pp_print_space Parsing.pp_block_info) bil; *)

    let p = Prog.of_module (Env.init bil to_loc) m in
    dbg_pr "pysem ast" "%a" Ast.pp_prog p;

    let ml = Prog.to_ml p in
    dbg_pr "mlsem ast" "%a"
      (pp_print_list ~pp_sep:pp_print_newline pp_ml_top) ml;

    let ms_exprs = List.map (fun (v,t) -> v, ML.Transform.transform t) ml in

    (* MS.Config.infer_overload := false; *)
    let tt, mce, names =
      try List.fold_left treat_def (0.,MC.Env.empty, []) ms_exprs
      with
      | MSC.Untypeable err ->
        Format.printf "@[<hov>%s:@\n@ @[<hov 2>%a@]@]" err.title
          (Format.pp_print_option pp_print_string) err.descr;
        raise (MSC.Untypeable err)
    in
    dbg_pr "reconstruction environement" "";
    names
    |> List.rev
    |> List.iter (fun v ->
        let s = MC.Env.find v mce in
        if MlVar.show v |> Utils.is_internal |> not then
          Format.printf "@[<hov>%a: %a@]@\n"
            MlVar.pp v
            Py_params.pp_py_scheme s
        else
        if Utils.debug then
          Format.printf "@[<hov>%a@]@\n"
            Printing.pp_ml_tys (v, s)
      );
    Format.printf "@{<yellow;italic>checked in %.2fms@}@\n"
      (ms_of_us tt)

let () =
  if Unix.isatty Unix.stdout then Colors.add_ansi_marking Format.std_formatter;
  let main = MT.PEnv.(sequential_handler empty main) in
  try
    if Utils.debug
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
  | MSC.Untypeable _ -> exit 2 (* printed above *)
  | e ->
    Format.eprintf "ERROR: %s@\n%s@\n"
      (Printexc.to_string e)
      (Printexc.get_backtrace ());
    exit 10
