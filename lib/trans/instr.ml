open Aliases
open Utils
open Ast

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


let no_var ast = dummy_var_t, ast

let rec to_ml (p,instr:instr) : (MlMVar.t * MLAst.t) =
  match instr with
  | Block l ->
     begin match l with
     | [] -> mk_unit p |> no_var
     | [i] -> to_ml i
     | _ -> failwith "TODO"
     end
  | FunDef (f,args,body) ->
     let [@warning "-26"] pp_recty fmt fbt_ll =
       Format.(
         fprintf fmt "@[%a@]"
           (Printing.pp_list
              (fun fmt fbt_l ->
                fprintf fmt "@[<hov 2>[ %a@ ]@]"
                  (Printing.pp_list
                     (fun fmt (f,(b,_t)) ->
                       fprintf fmt "@[%s :%s %a@]" f
                         (if b then "?" else "") MT.Ty.pp _t) )
                  fbt_l) )
           fbt_ll)
     in

     let f_arg_v = mk_var_t ~kind:MlMVar.Immut ml_fun_arg_name in
     let f_arg = var_of_vart p f_arg_v in
     let nb_pos = List.length args.posonly
     and nb_arg = List.length args.args in

     let get_pos i = mk_projection p (MSAst.Field (field_name_pos i)) f_arg in
     let get_kw id = mk_projection p (MSAst.Field (field_name_kw id)) f_arg in

     let load_arg (pak:[`Pos|`Arg|`Kwd]) (i,d,l,t) (id,eo) =
       let opt, eo, default = match eo with
         | None -> false, None, d
         | Some e ->
            let e = mk_var_t ~kind:MlMVar.Immut
                      (Ident.show id |> def_var_name)
                  , Expr.to_ml e in
            true, Some e, e::d in
       let id_kw = Ident.show id in
       let t, ast_in = match pak with
         | `Pos ->
            let field = field_name_pos i in
            let tv = field |> mk_tv in
            ( List.map (fun rec_t -> (field,(opt,tv))::rec_t) t
            , match eo with
              | None -> get_pos i
              | Some (v,(pos,_)) ->
                 let g = Builtins.getter_pk field |> var_of_vart p in
                 mk_tuple p [ f_arg; (var_of_vart (MC.Eid.loc pos) v) ]
                 |> mk_app p g )
         | `Arg ->
            let field_p = field_name_pos i in
            let field_k = field_name_kw id_kw in
            let field_a = field_name_arg i id_kw in
            let tv = mk_tv field_a in
            let g = Builtins.getter_a i id_kw field_p field_k field_a opt
                    |> var_of_vart p in
            let get = match eo with
              | None -> mk_app p g f_arg
              | Some (v,(pos,_)) ->
                 mk_tuple p [ f_arg; (var_of_vart (MC.Eid.loc pos) v) ]
                 |> mk_app p g in
            ( List.(mapi (fun j rec_t ->
                        if j < length t - (i-nb_pos) - 1
                        then (field_p,(opt,tv))::rec_t
                        else (field_k,(opt,tv))::rec_t)
                      t)
            , get )
         | `Kwd ->
            let field = field_name_kw id_kw in
            let tv = field |> mk_tv in
            ( List.map (fun rec_t -> (field,(opt,tv))::rec_t) t
            , match eo with
              | None -> get_kw id_kw
              | Some (v,(pos,_)) ->
                 let g = Builtins.getter_pk field |> var_of_vart p in
                 mk_tuple p [ f_arg; (var_of_vart (MC.Eid.loc pos) v) ]
                 |> mk_app p g )
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
     let f_body = join_let_rev p preamble (to_ml body |> snd) in
     let f_anon = mk_lambda p [] f_type f_arg_v f_body in
     ( f.name
     , join_let_rev p def f_anon )
  | Return eo ->
     ( match eo with None -> mk_unit p | Some e -> Expr.to_ml e )
     |> mk_return p |> no_var
  | Assign _ -> failwith "TODO"
  | While _ -> failwith "TODO"
  | If _ -> failwith "TODO"
  | Iexpr e -> Expr.to_ml e |> no_var
  | Break | Continue -> mk_break p |> no_var
