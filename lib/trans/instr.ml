open Mlsem_app
open Mlsem
open Ast

module PC = PyreAst.Concrete

let rec of_statement env (stmt:PC.Statement.t) : instr =
  let annot = env_annot env in
  match stmt with
  | FunctionDef r ->
     let fenv = (* new scope → new environement *)
       let open Parsing in
       let bi = BlockId.mk_fun (PC.Identifier.to_string r.name) r.location in
       { env with current = BidTable.find env.infos bi } in
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

module MC = Mlsem.Common
module MlMVar = Mlsem_lang.MVariable
module MlAst = Mlsem_lang.Ast
module MSAst = Mlsem_system.Ast
module MlT = Mlsem.Types
module MlGTy = Mlsem.Types.GTy

let ml_annot p (ast:MlAst.e) = (MC.Eid.unique_with_pos p, ast)

let dummy_ml_ast = MlAst.Value (MlGTy.any) |> ml_annot MC.Position.dummy
let ml_fun_arg_name = "%#rec_arg"

let to_ml (p,instr:instr) : MlAst.t =
  match instr with
  | Block _ -> failwith "TODO"
  | FunDef (f,args,_body) ->
     let mk_vart ?(kind=MlMVar.Mut) str = MlMVar.create kind (Some str) in
     let mk_var ?(pos=p) ?(kind=MlMVar.Mut) str =
       Var (mk_vart ~kind str) |> ml_annot pos in
     let var_of_vart ?(pos=p) v = Var v |> ml_annot pos in
     let mk_projection ?(pos=p) proj ast =
       MlAst.Projection (proj, ast) |> ml_annot pos in
     let mk_lambda ?(pos=p) ty ?(gty=MlGTy.any) id body =
       MlAst.Lambda (ty, gty, id, body) |> ml_annot pos in
     let mk_let ?(pos=p) ?(ty=[]) id ast_in ast_out =
       MlAst.Let (ty,id,ast_in,ast_out) |> ml_annot pos in
     let mk_ite ?(pos=p) test ty thn els =
       MlAst.Ite (test,ty,thn,els) |> ml_annot pos in

     let f_var = mk_vart f.name in
     let f_rec_arg = mk_vart ~kind:MlMVar.Immut ml_fun_arg_name in
     let f_arg = mk_var ml_fun_arg_name in

     let arg_name_pos i = Utils.mk_id "#p%d" i
     and arg_name_kw  k = Utils.mk_id "#k%s" k
     and def_arg_name k = Utils.mk_id "#d%s" k in
     let get_pos i = mk_projection (MSAst.Field (arg_name_pos i)) f_arg in
     let get_kw id = mk_projection (MSAst.Field (arg_name_kw id)) f_arg in

     let load_arg (pak:[`Pos|`Arg|`Kwd]) (i,d,l) (id,eo) =
       let ast_in = match pak with
         | `Pos -> get_pos i
         | `Arg ->
            mk_ite f_arg MlT.(
             Record.mk true [ arg_name_pos i
                            , (false, TVar.(mk KInfer (Some (arg_name_pos i))
                                            |> typ))] )
                     (get_pos i) (get_kw id.name)
         | `Kwd -> get_kw id.name
       in
       let default = match eo with
         | None -> d
         | Some _e -> ( mk_vart ~kind:MlMVar.Immut (def_arg_name id.name)
                      , failwith "Expr.to_ml _env e" )::d
       in
       ( i+1
       , default
       , ( mk_vart id.name
         , ast_in
         ) ::l )
     in
     let i, def_po, preamble_po =
       List.fold_left (load_arg `Pos) (0,[],[]) args.posonly  in
     let _, def_po_args, preamble_po_args =
       List.fold_left (load_arg `Arg) (i,def_po, preamble_po) args.args in
     let f_type = [] in

     let join_let last var_in =
       List.fold_left (fun body (v,e) -> mk_let v e body) last var_in in
     let f_body = join_let dummy_ml_ast (* TODO:body *) preamble_po_args in
     let f_anon = mk_lambda f_type f_rec_arg f_body in
     let f_w_defaults = join_let f_anon def_po_args in
     mk_let
       f_var
       f_w_defaults
       (var_of_vart f_var)
  | Return _ -> failwith "TODO"
  | Assign _ -> failwith "TODO"
  | While _ -> failwith "TODO"
  | If _ -> failwith "TODO"
  | Iexpr _ -> failwith "TODO"
  | Break -> MlAst.Break |> ml_annot p
  | Continue -> failwith "TODO"

(* === === === === === === *)

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
