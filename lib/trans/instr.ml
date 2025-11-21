open Ast
open Utils.Aliases
open Utils

let rec of_statement env (stmt:PC.Statement.t) : instr =
  let annot = env_annot env in
  match stmt with
  | FunctionDef r ->
     let fenv = (* new scope → new environement *)
       let open Parsing in
       let bi = BlockId.mk_fun (PCI.to_string r.name) r.location in
       let current = BidTable.find env.infos bi in
       let vars =
         IdentMap.fold
           ( fun py_ident py_info vmap ->
             IdentMap.add py_ident
               ( begin match py_info.scope with
                 | Local | Parameter ->
                    mk_var_t (PCI.to_string py_ident)
                 | Nonlocal | Global ->
                    IdentMap.find py_ident env.vars |> fst
                 | Unknown -> assert false
                 end
               , py_info)
               vmap)
           current.identifiers env.vars in
       { env with current; vars } in
     let spec = Expr.spec_of_arguments fenv r.args in
     let body = Block (List.map (of_statement fenv) r.body)
                |> annot r.location in
     FunDef ( Ident.of_identifier env r.name
            , spec
            , body )
     |> annot r.location
  (* | AsyncFunctionDef | ClassDef *)
  | Return r -> Return (Option.map (Expr.of_expression env) r.value)
                |> annot r.location
  (* | Delete *)
  | Assign ({targets=[Name {id=x;_}];_} as r) ->
     let x = Ident.of_identifier env x in
     let e = Expr.of_expression env r.value in
     Assign (x,e) |> annot r.location
  | Assign _ -> failwith "Not implemented (Instr.Assign(¬ only 1 target var))."
  (* | TypeAlias | AugAssign | AnnAssign | For | AsyncFor *)
  | While ({orelse=[];_} as r) ->
     let e = Expr.of_expression env r.test in
     let is = List.map (of_statement env) r.body in
     While (e,Block is |> annot r.location) |> annot r.location
  | While _ -> failwith "Not implemented (Instr.While(orelse))"
  | If r ->
     let test = Expr.of_expression env r.test in
     let thn = Block (List.map (of_statement env) r.body  ) |> annot r.location in
     let els = match List.map (of_statement env) r.orelse with
       | [] -> Option.None
       | l -> Some (Block l |> annot r.location) in
     If (test,thn,els) |> annot r.location
  (* | With | AsyncWith | Match | Raise | Try | TryStar | Assert | Import
     | ImportFrom  *)
  | Global {location;_} | Nonlocal {location;_} | Pass {location}
    -> Block [] |> annot location (* or Expr.None ? *)
  | Expr r -> Iexpr (Expr.of_expression env r.value) |> annot r.location
  | Break {location} -> annot location Break
  | Continue {location} -> annot location Continue

  | AsyncFunctionDef _ | ClassDef _ | Delete _ | TypeAlias _ | AugAssign _
    | AnnAssign _ | For _ | AsyncFor _ | With _ | AsyncWith _ | Match _
    | Raise _ | Try _ | TryStar _ | Assert _ | Import _ | ImportFrom _
    -> failwith "Not implemented (Instr)."


let dummy_ml_ast = MlAst.Value (MlGTy.any) |> ml_annot MC.Position.dummy

let rec to_ml (p,instr:instr) : MlAst.t =
  match instr with
  | Block _ -> failwith "TODO"
  | FunDef (f,args,body) ->
     let [@warning "-26"] pp_recty fmt fbt_ll =
       Format.(
         fprintf fmt "@[%a@]"
           (pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt ";@\n")
              (fun fmt fbt_l ->
                fprintf fmt "@[<hov 2>[ %a@ ]@]"
                  (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ")
                     (fun fmt (f,(b,_t)) ->
                       fprintf fmt "@[%s:(%a,%a)@]" f
                         pp_print_bool b MT.Ty.pp _t) )
                  fbt_l) )
           fbt_ll)
     in
     let ml_fun_arg_name = "%rec_arg" in

     let f_arg_v = mk_var_t ~kind:MlMVar.Immut ml_fun_arg_name in
     let f_arg = var_of_vart p f_arg_v in
     let nb_pos = List.length args.posonly
     and nb_arg = List.length args.args in

     let get_pos i = mk_projection p (MSAst.Field (arg_name_pos i)) f_arg in
     let get_kw id = mk_projection p (MSAst.Field (arg_name_kw id)) f_arg in

     let mk_ite_rectest field _tv thn els =
       mk_ite p f_arg
         MT.(Record.mk true
                [ field, (false,MT.Ty.any) ])
         thn els
     in
     let get_or_def mlget strget i_kw otv eo = match eo with
       | None -> mlget i_kw
       | Some (v,(pos,_)) -> mk_ite_rectest (strget i_kw) otv
                               (mlget i_kw)
                               (var_of_vart (MC.Eid.loc pos) v)
     in
     let load_arg (pak:[`Pos|`Arg|`Kwd]) (i,d,l,t) (id,eo) =
       let opt, eo, default = match eo with
         | None -> false, None, d
         | Some e ->
            let e = mk_var_t ~kind:MlMVar.Immut
                      (Ident.show id |> def_arg_name)
                  , Expr.to_ml e in
            true, Some e, e::d in
       let id_kw = mlvar_get_name id.name in
       let t, ast_in = match pak with
         | `Pos ->
            let field = arg_name_pos i in
            let tv = field |> mk_tv in
            ( List.map (fun rec_t -> (field,(opt,tv))::rec_t) t
            , get_or_def get_pos arg_name_pos i tv eo )
         | `Arg ->
            let field_p = arg_name_pos i in
            let field_k = arg_name_kw id_kw in
            let tv = arg_name_arg id_kw |> mk_tv in
            ( List.(mapi (fun j rec_t ->
                        if j < length t - (i-nb_pos) - 1
                        then (field_p,(opt,tv))::rec_t
                        else (field_k,(opt,tv))::rec_t)
                      t)
            , mk_ite_rectest (arg_name_pos i) tv
                (get_pos i)
                (get_or_def get_kw arg_name_kw id_kw tv eo) )
         | `Kwd ->
            let field = arg_name_kw id_kw in
            let tv = field |> mk_tv in
            ( List.map (fun rec_t -> (field,(opt,tv))::rec_t) t
            , get_or_def get_kw arg_name_kw id_kw tv eo )
       in
       ( i+1
       , default
       , (id.name, ast_in)
         ::l
       , t )
     in
     let ty = List.init (nb_arg+1) (fun _ -> []) in
     let i, def, preamble, ty =
       List.fold_left (load_arg `Pos) (0, [] , []      , ty) args.posonly in
     let i, def, preamble, ty =
       List.fold_left (load_arg `Arg) (i, def, preamble, ty) args.args    in
     let _, def, preamble, ty =
       List.fold_left (load_arg `Kwd) (i, def, preamble, ty) args.kwonly  in
     let ty = List.(map rev ty) in
     (* dbg_pr "rectype" pp_recty ty; *)
     let sstt_ty = mk_rec_disj false ty in
     (* dbg_pr "sstt_ty" MT.Ty.pp sstt_ty; *)
     let f_type = MlGTy.mk sstt_ty in
     (* dbg_pr "gty" MlGTy.pp f_type; *)
     let join_let_rev pos var_in last =
       List.fold_left (fun body (v,e) -> mk_let pos [] v e body) last var_in in
     let f_body = join_let_rev p preamble (to_ml body) in
     let f_anon = mk_lambda p [] f_type f_arg_v f_body in
     let f_w_defaults = join_let_rev p def f_anon in
     mk_let p []
       f.name
       f_w_defaults
       (var_of_vart p f.name)
  | Return eo ->
     ( match eo with None -> mk_unit p | Some e -> Expr.to_ml e )
     |> mk_return p
  | Assign _ -> failwith "TODO"
  | While _ -> failwith "TODO"
  | If _ -> failwith "TODO"
  | Iexpr e -> Expr.to_ml e
  | Break | Continue -> MlAst.Break |> ml_annot p

(* === === === === === === *)
open Mlsem_app
open Mlsem

let dummy_def = PAst.Definitions []
let dummy_ast = PAst.Tuple []
let dummy_exp = (PAst.new_annot Common.Position.dummy, dummy_ast)

let translate_stmt _env _stmt : PAst.parser_expr = failwith "TODO"

let translate_top env stmt : PAst.(annotation * parser_element) =
  let ast_to_t loc ast = env.Env.to_loc loc |> PAst.new_annot, ast in
  let open PC.Statement in
  match stmt with
  | FunctionDef r ->
     (* def f(a,b=bdef,*,k=kdef):
            ...
        ↓

        let f = (* or let env = {env with f = … } *)
          let b_ = [bdef] in
          let k_ = [kdef] in
          let f = fun r ->
            let a = r.#0 in
            let b = (r∈{#1;..})? r.#1 : (r∈{b;..})?r.b:b_ in
            let k = (r∈{ k;..})? r.k  : k_ in
            [...]
          in
          f
      *)

     let mlarg_str = "%#rec_arg" in
     let mlarg = PAst.(Var mlarg_str |> ast_to_t r.location) in
     let mk_var loc id =
       PAst.Var (PC.Identifier.to_string id) |> ast_to_t loc
     and mk_lambda loc var body =
       PAst.Lambda (var, None, body) |> ast_to_t loc
     and mk_let loc id expin exp =
       (* TODO monadic: update environement *)
       PAst.Let ( (Immut, PC.Identifier.to_string id)
                , expin
                , exp) |> ast_to_t loc
     and _mk_ite loc test ty thn els =
       PAst.Ite (test, ty, thn, els) |> ast_to_t loc
     in
     let load_args (p_args:Arguments.proto) body =
       let add_varinit expr (a,i,_eo,_tv) =
         let loc = a.PC.Argument.location in
         mk_let loc
           a.PC.Argument.identifier
           PAst.(Projection (Field (Arguments.pos_param_name i), mlarg)
                 |> ast_to_t loc)
           expr
       in
       let rpos, _rargs, _rkw = List.( rev p_args.pos_only
                                     , rev p_args.args
                                     , rev p_args.kw_only) in
       List.fold_left add_varinit body rpos
     and fun_def loc f_id preamble_and_body =
       mk_let loc
         f_id
         (mk_lambda loc mlarg_str preamble_and_body)
         (mk_var loc f_id)
     and default_init _p_args f_def : PAst.parser_expr =
       f_def
     in
     let body = dummy_exp in
     let proto = Arguments.translate_arguments r.args in
     let annot = PAst.new_annot (env.Env.to_loc r.location) in
     let expr = default_init proto
                  (fun_def r.location r.name
                     (load_args proto body)) in
     let defs = [( (PAst.Immut, PC.Identifier.to_string r.name)
                 , expr)] in
     let () = Format.printf "Function %s: @[%a@]@\n"
                (PC.Identifier.to_string r.name)
                Arguments.pp_proto proto in
     let () = Format.printf "result:@.  @[%a@]@\n%!"
                Ast.PAstPrinter.pp_t expr in
     (annot, PAst.Definitions defs)
  | _ -> PAst.new_annot Common.Position.dummy, dummy_def
