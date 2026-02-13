[@@@warning "-32-33"]

open Pysem
open Aliases
open Printing

module MSC = MS.Checker
module MTS = MT.TyScheme

(* TRANSLATE AND TYPE *)

let ml_type mcenv ast =
  let annot = MS.Reconstruction.infer mcenv
      (MS.Refinement.refinements mcenv ast) ast in
  MSC.typeof_def mcenv annot ast
  |> MTS.norm_and_simpl

let upd_env mce v ts =
  ( if MC.Env.mem v mce
    then
      (* if !Utils.shadowing *)
      (* then MC.Env.rm v mce *)
      (* else *)
      failwith (Format.sprintf "Cannot add '%s' twice to environement!"
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

let treat_file file =
  let open Format in
  let m, globals, bil, to_loc = Parsing.parse ~file in
  (* dbg_pr "pyre-parsed expression" "%a" *)
  (* Sexplib0.Sexp.pp_hum (PC.Module.sexp_of_t m); *)
  (* pr "block_infos" "%a" *)
  (* (pp_list ~sep:"@\n" Parsing.pp_block_info) bil; *)

  Env.(set_mut_top false; set_mut_local false);

  let p = Prog.of_module (Env.init globals bil to_loc) m in
  dbg_pr "pysem ast" "%a" Ast.pp_prog p;

  let e = Py_state.of_prog p in
  dbg_pr "pystate ast" "%a" (pp_list ~sep:"@\n" Py_state.pp_expr) e;

  let er = List.map Py_state.reduce e in
  pr "reduced pystate ast" "%a" (pp_list ~sep:"@\n" Py_state.pp_expr) er;

  let ml_er = List.map Py_state.to_ml er in
  pr "ml reduced pst ast" "%a"
    (pp_list ~sep:"@\n" Printing.MLAstPrinter.pp_t) ml_er;

  let mlsys = List.map ML.Transform.transform ml_er in
  pr "mlsys reduced pst ast" "%a"
    (pp_list ~sep:"@\n" Printing.MSAstPrinter.pp_t) mlsys;

  let v_t = List.map (fun t -> MlVar.create None, t) mlsys in
  let tt, mce, names = List.fold_left treat_def (0.,MC.Env.empty, []) v_t in

  (* * )
  let ml = Prog.to_ml p in
  dbg_pr "mlsem ast" "%a"
    (pp_list ~sep:"@\n" pp_ml_top) ml;

  let ms_exprs = List.map (fun (v,t) -> v, ML.Transform.transform t) ml in

  (* MS.Config.infer_overload := false; *)
  let tt, mce, names = List.fold_left treat_def (0.,MC.Env.empty, []) ms_exprs in

  ( * *)

  pr "reconstruction environement"
    "%a@\n@{<yellow;italic>checked in %.2fms@}"
    (let nl = ref true in
     pp_print_list
       ~pp_sep:(fun fmt _ -> if !nl then fprintf fmt "@\n"; nl := true)
       ( fun fmt v ->
           let s = MC.Env.find v mce in
           if MlVar.show v |> Utils.is_internal |> not
           then Format.fprintf fmt "@[<hov>%a: %a@]"
               MlVar.pp v
               Py_params.pp_py_scheme s
           else if !Utils.debug
           then Format.fprintf fmt "@[<hov>%a@]"
               Printing.pp_ml_tys (v, s)
           else nl := false ))
    (List.rev names)
    (ms_of_us tt) (* *)

(* CLI *)

let usage_message = Format.sprintf "%s <file.py>" Sys.argv.(0)

let input_file = ref None

let add_input_file s =
  match !input_file with
  | None -> input_file := (Some s)
  | Some _ -> raise (Arg.Bad "multiple files provided")

let options =
  Arg.align
    [ "-debug" , Arg.Set Utils.debug , " Print debug information"
    ; "-export", Arg.Set Utils.export, " Print code without illegal characters" ]

let set_env_vars () =
  Utils.user_vars
  |> List.iter (fun (ref_v, env_var) ->
      Sys.getenv_opt env_var
      |> Option.iter (fun str ->
          List.assoc_opt str Utils.sh_values
          |> Option.iter (fun v -> ref_v := v)))

(* ENTRY POINT *)

let main () =
  set_env_vars ();
  Arg.parse options add_input_file usage_message;
  match !input_file with
  | None ->
    Format.eprintf "%s: missing file@\n%s" Sys.argv.(0)
      (Arg.usage_string options usage_message)
  | Some file ->
    let treat_file = MT.PEnv.(sequential_handler empty treat_file) in
    if !Utils.debug
    then begin
      MT.Recording.start_recording ();
      treat_file file |> fst;
      MT.Recording.stop_recording ();
      MT.Recording.(tally_calls () |> save_to_file "tally_calls");
    end
    else treat_file file |> fst

let () =
  if Unix.isatty Unix.stdout then Colors.add_ansi_marking Format.std_formatter;
  try main () with
  | Parsing.Syntax (file, e) ->
    Format.eprintf "%s: %d:%d-%d:%d : %s@\n"
      file e.line e.column e.end_line e.end_column e.message;
    exit 3
  | Sys_error msg -> Format.eprintf "%s@\n" msg; exit 1
  | MSC.Untypeable err ->
    let pos = MC.Eid.loc err.eid in
    let start_p = MC.Position.start_of_position pos in
    let end_p = MC.Position.end_of_position pos in
    let message = match err.descr with None -> "" | Some s -> " (" ^ s ^ ")" in
    Format.eprintf "%s: %d:%d-%d:%d : %s%s@\n"
      start_p.pos_fname
      start_p.pos_lnum
      (start_p.pos_cnum - start_p.pos_bol + 1)
      end_p.pos_lnum
      (end_p.pos_cnum - end_p.pos_bol + 1)
      err.title message;
    exit 2 (* printed above *)
  | e ->
    Format.eprintf "ERROR: %s@\n%s@\n"
      (Printexc.to_string e)
      (Printexc.get_backtrace ());
    exit 10
