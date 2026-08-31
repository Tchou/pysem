[@@@warning "-32-33"]

open Pysem
open Aliases
open Printing

module MSC = MS.Checker
module MTS = MT.TyScheme

(* TRANSLATE AND TYPE *)

let ml_type mcenv ast =
  let annot = MS.Reconstruction.infer
      ~direct_narrowing:true ~partition_narrowing:false mcenv
      (MS.Refinement.refinements mcenv ast) ast in
  MSC.typeof_def mcenv annot ast
  |> MTS.simplify_factorize

let upd_env mce v ts =
  ( if MC.Env.mem v mce
    then failwith (Format.sprintf "Cannot add '%s' twice to environement!"
                     (mlvar_show v))
    else mce )
  |> MC.Env.add v ts

let ms_of_us t = t *. 1000.

let treat_def (tt, mce, lst) (v, ast) =
  let time0 = Unix.gettimeofday () in
  let v_str = MlVar.show v in
  if !Utils.debug then Format.printf "TYPING:%s\n%!" v_str;
  let ts = try ml_type mce ast with MSC.Untypeable err as msc_e ->
    if !Utils.debug
    then Format.printf "@{<bold>@{<red>Typing error@}:@}@\n%a@\n"
        Printing.MSAstPrinter.(err_pp err.eid) ast;
    raise msc_e
  in
  let _, gy = MTS.get ts in
  let ts = MTS.mk_poly gy in (* generalize? *)
  let ts = MTS.bot_instance ts in

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
  (* dbg_pr "pyre-parsed expression" "%a"
    Sexplib0.Sexp.pp_hum (PC.Module.sexp_of_t m); *)
  (* pr "block_infos" "%a" *)
  (* (pp_list ~sep:"@\n" Parsing.pp_block_info) bil; *)

  Env.(set_mut_top false; set_mut_local false);
  MS.Config.infer_overload := false;
  MS.Config.value_restriction := false;

  let (_,g as p) = Prog.of_module (Env.init globals bil to_loc) m in
  pr ~dbg:0 "pysem ast" "%a" Ast.pp_prog p;

  (* Then modify Py_state.(of_prog and make_state_record etc.) *)

  (* let vars = List.(
      fold_left
        (fun acc (bi:Parsing.block_info) ->
           (Parsing.IdentMap.to_list bi.identifiers |> List.split |> fst)
           @ acc)
        (Parsing.IdentMap.to_list globals |> List.split |> fst)
        bil
      |> map (fun pci ->
          let id = PCI.to_string pci in
          id, MT.TVar.(Some id |> mk KInfer |> typ, inner_let))
     ) in *)
  let module_state = Py_state.init_state g
    (* Env.vars_rec true
    |> List.map (fun (mlvar, (_ty, _b)) ->
        (mlvar, (Utils.undef, false)))
       |> MT.Record.mk_closed |> MlGTy.mk *) in
  let module_state_t = MlGTy.mk module_state in

  let e = Py_state.of_prog p in
  dbg_pr "pystate ast" "%a" (pp_list ~sep:"@\n" Py_state.pp_expr) e;

  let e = Py_state.prepare_toplevel e in
  let er = List.map (fun (v, e) -> (v, Py_state.reduce e)) e in
  pr "reduced pystate ast" "%a"
    (pp_list ~sep:"@\n" Py_state.pp_expr) (List.map snd er);

  let ml_er = List.map (fun (v,e) ->
      let p,mle as ml = Py_state.to_ml e in
      v, (p, if Py_state.cast_toplevel
          then MLAst.TypeCoerce (ml, module_state_t, MSAst.Check)
          else mle)
    ) er in
  let ml_er = match ml_er with
    | [] -> []
    | (s0,(p,_))::l -> (s0,(p,MLAst.Value module_state_t))::l in
  let ml_er =
    if Py_state.pack_toplevel
    then match List.rev ml_er with
      | [] -> []
      | (_,e)::l -> [ MlVar.create (Some "final_state")
                    , Utils.join_let_rev l e]
    else ml_er in

  pr "ml reduced pst ast" "%a"
    (pp_list ~sep:"@\n" Printing.MLAstPrinter.pp_t) (List.map snd ml_er);

  let ml_er =
    (Py_state.PSBuiltins.all ()
     |> List.map (fun (mlv,ty) ->
         mlv, MlGTy.mk ty |> Utils.mk_value MC.Position.dummy))
    @ ml_er in

  let v_t = List.map (fun (v,t) -> v, ML.Transform.transform t) ml_er in
  dbg_pr "mlsys reduced pst ast" "%a"
    (pp_list ~sep:"@\n"
       (fun fmt (v,t) ->
          Format.fprintf fmt "@[%a: @[%a@]@]@."
            Printing.MSAstPrinter.pp_variable v
            Printing.MSAstPrinter.pp_t t)) v_t;

  (* let v_t = List.map (fun t -> MlVar.create None, t) mlsys in *)
  (* MS.Config.infer_overload := false; *)
  let tt, mce, names = List.fold_left treat_def (0., MC.Env.empty, []) v_t in

  (* * )

     let ml = Prog.to_ml p in
     dbg_pr "mlsem ast" "%a"
     (pp_list ~sep:"@\n" pp_ml_top) ml;

     let ms_exprs = List.map (fun (v,t) -> v, ML.Transform.transform t) ml in

     let tt, mce, names = List.fold_left treat_def
         (0.,MC.Env.empty, []) ms_exprs in

  ( * *)

  let names =
    (* if !Utils.sumup then match names with [] -> [] | x::_ -> [x] else *)
      List.rev names in
  pr ~dbg:0 "reconstruction environement"
    "@{<yellow;italic>checked in %.2fms@}@\n%a"
    (ms_of_us tt)
    (let nl = ref true in
     pp_print_list
       ~pp_sep:(fun fmt _ -> if !nl then fprintf fmt "@\n"; nl := true)
       ( fun fmt v ->
           let s = MC.Env.find v mce in
           if true
           (* MlVar.show v |> Utils.is_internal |> not *)
           then Format.fprintf fmt "@[<hov>%a: %a@]"
               MlVar.pp v
               Py_params.pp_py_scheme s
           else if !Utils.debug
           then Format.fprintf fmt "@[<hov>%a@]"
               Printing.pp_ml_tys (v, s)
           else nl := false ))
    names

  (* *)

(* CLI *)

let usage_message = Format.sprintf "%s [options] <file.py>" Sys.argv.(0)

let input_file = ref []

let add_input_file s =
  match !input_file with
  | [] -> input_file := [s]
  | l -> input_file := s::l

let options =
  Arg.align
    [ "-debug" , Arg.Set Utils.debug , " Print debug information"
    ; "-sumup" , Arg.Set Utils.sumup , " Print only essential information"
    ; "-export", Arg.Set Utils.export, " Print code without illegal characters"
    ]

let set_env_vars () =
  Utils.user_vars
  |> List.iter (fun (ref_v, env_var) ->
      Sys.getenv_opt env_var
      |> Option.iter (fun str ->
          List.assoc_opt str Utils.sh_values
          |> Option.iter (fun v -> ref_v := v)))

exception Sigint

let backwards = ref ""

(* ENTRY POINT *)

let check ?(suf="") file =
  let treat_file = MT.PEnv.(sequential_handler empty treat_file) in
  try
    if !Utils.debug
    then begin
      MT.Recording.(
        clear ();
        start_recording () );
      treat_file file |> fst;
      MT.Recording.(
        stop_recording ();
        tally_calls () |> save_to_file ("tally_calls" ^ suf) );
    end
    else treat_file file |> fst;
    0
  with
  | Parsing.Syntax (file, e) ->
    Format.eprintf "@{<bold;red>Syntax error@} %s: %d:%d-%d:%d : %s@\n%!"
      file e.line e.column e.end_line e.end_column e.message;
    2
  | MSC.Untypeable err ->
    let pos = MC.Eid.loc err.eid in
    let start_p = MC.Position.start_of_position pos in
    let end_p = MC.Position.end_of_position pos in
    let message = match err.descr with None -> "" | Some s -> s in
    Format.eprintf
      "@{<bold;red>%s@} at %s: %d:%d-%d:%d :@\n%s@\n%!"
      err.title start_p.pos_fname
      start_p.pos_lnum (start_p.pos_cnum - start_p.pos_bol + 1)
      end_p.pos_lnum (end_p.pos_cnum - end_p.pos_bol + 1)
      message;
    1
  | Sigint -> 1

let () =
  if Unix.isatty Unix.stdout
  then begin
    Colors.add_ansi_marking Format.std_formatter;
    Colors.add_ansi_marking Format.err_formatter;
    backwards := "\x08\x08";
    match Terminal_size.get_columns () with
    | None -> () | Some i -> Format.set_margin (i-1)
  end;
  Sys.(Signal_handle (fun _ ->
      Format.eprintf "%s@{<bold;red>Interrupted…@}@\n%!" !backwards;
      raise Sigint)
     |> set_signal sigint);
  try
    set_env_vars ();
    Arg.parse options add_input_file usage_message;
    input_file := List.rev !input_file;
    let ecode = match !input_file with
      | [] ->
        Format.eprintf "%s: missing file@\n%s" Sys.argv.(0)
          (Arg.usage_string options usage_message);
        2
      | [file] -> check file
      | l ->
        let total, ok, ecode =
          List.fold_left (fun (i,ok,exit_code) file ->
              if i <> 0 then Printing.new_file ();
              pr ~dbg:0 "" "@{<bold>File:@} %s" file;
              let c = check ~suf:(string_of_int i) file in
              i+1, ok + (if c = 0 then 1 else 0), max exit_code c )
            (0,0,0) l in
        pr ~dbg:0 "" "@.@{<bold>Total:@} %d/%d passed" ok total;
        ecode
    in
    if ecode > 0 then exit ecode
  with
  | Sys_error msg -> Format.eprintf "%s@\n%!" msg; exit 1
  | e ->
    Format.eprintf "ERROR: %s@\n%s@\n%!"
      (Printexc.to_string e)
      (Printexc.get_backtrace ());
    exit 10
