open Aliases
open Ast

let zip_for l1 l2 =
  let rec loop l1 l2 acc =
    match l1, l2 with
    | [], l2 -> acc, l2
    | e1 :: ll1, e2 :: ll2 -> loop ll1 ll2 ((e1, Some e2)::acc)
    | e1 :: ll1, []        -> loop ll1 []  ((e1, None   )::acc)
  in
  loop l1 l2 []

let rec of_expression (env:Env.t) (e:PC.Expression.t) : expr =
  let annot = env_annot env in
  match e with
  | BoolOp ({values=[a;b];_} as r) ->
     Binop ( of_expression env a
           , Binop.of_boolop r.op
           , of_expression env b) |> annot r.location
  | BoolOp _ -> failwith "Not implemented (Expr.BoolOp(values<>[a;b]))."
  (* | NamedExpr *)
  | BinOp r -> Binop ( of_expression env r.left
                     , Binop.of_binop r.op
                     , of_expression env r.right) |> annot r.location
  (* | UnaryOp *)
  | Lambda r ->
    let bid =  Parsing.BlockId.mk_lambda r.location in
    let lenv = Env.upd env bid in
    let si = Ast.scoped_identifiers lenv bid in
    Lambda ( spec_of_arguments lenv r.args
           , si
           , of_expression lenv r.body )
    |> annot r.location
  (* | IfExp | Dict | Set | ListComp | SetComp | DictComp | GeneratorExp | Await
     | Yield | YieldFrom *)
  | Compare ({ops=[op];comparators=[right];_} as r) ->
     Binop ( of_expression env r.left
           , Binop.of_comparisonoperator op
           , of_expression env right) |> annot r.location
  | Compare _ -> failwith "Not implemented (Expr.Compare(¬ only 2 arguments))."
  | Call ({func=Name _;_} as r) ->
     let kwd =
       List.map
         (fun kwd ->
           let open PC.Keyword in
           match kwd.arg with
           | Some id -> ( PCI.to_string id
                        , of_expression env kwd.value )
           | None -> failwith "Not implemented (Expr.Call(several kw_args)).")
         r.keywords in
     Apply ( of_expression env r.func
           , { pos = List.map (of_expression env) r.args
             ; kwd } )
     |> annot r.location
  | Call _ -> failwith "Not implemented (Expr.Call(complex expression))."
  (* | FormattedValue | JoinedStr *)
  | Constant r -> Cst (Const.of_constant r.value) |> annot r.location
  (* | Attribute *)
  (* | Subscript r ->
     Projection (of_expression env r.slice, of_expression env r.value)
     |> annot r.location *)
  (* | Starred *)
  | Name r -> Var (Ident.of_identifier env r.id) |> annot r.location
  (* | List *)
  | Tuple r -> Tuple (List.map (of_expression env) r.elts) |> annot r.location
  (* | Slice *)

  | NamedExpr _ | UnaryOp _ | IfExp _ | Dict _ | Set _ | ListComp _ | SetComp _
    | DictComp _ | GeneratorExp _ | Await _ | Yield _ | YieldFrom _
    | FormattedValue _ | JoinedStr _ | Attribute _ | Subscript _ | Starred _
    | List _ | Slice _
    -> failwith "Not implemented (Expr)."

and spec_of_arguments fenv (a:PC.Arguments.t) =
  let mixed, rem_args = zip_for (List.rev a.args) (List.rev a.defaults) in
  let posonly, rem_pos = zip_for (List.rev a.posonlyargs) rem_args in
  assert (rem_pos = []);
  let kwdonly = List.combine a.kwonlyargs a.kw_defaults in
  let format (arg,eo) = Ident.of_argument fenv arg
                      , Option.map (of_expression fenv) eo in
  { posonly = List.map format posonly
  ; mixed   = List.map format mixed
  ; kwdonly = List.map format kwdonly
  ; vararg  = Option.map (Ident.of_argument fenv) a.vararg
  ; kwdarg  = Option.map (Ident.of_argument fenv) a.kwarg }

open Utils

let rec ml_lambda p args body =
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

  let load_arg (pmk:[`Pos|`Mix|`Kwd]) (i,d,l,t) (id,eo) =
    let opt, eo, default = match eo with
      | None -> false, None, d
      | Some e ->
         let (pos, _) as e_def = to_ml e in
         let v = mk_var_t ~kind:MlMVar.Immut
                   (Ident.external_name id |> def_var_name)
         in
         let e_var = var_of_vart (MC.Eid.loc pos) v in
         true, Some e_var, (v,e_def)::d
    in
    let tv = Ident.external_name id |> mk_tv in
    let id_kw = Ident.external_name id in
    let t, ast_in = match pmk with
      | `Pos ->
         ( Py_params.add_param t `Pos i id_kw tv opt
         , Py_params.pos_getter p i f_arg eo)
      | `Mix ->
         ( Py_params.add_param t `Mix i id_kw tv opt
         , Py_params.mix_getter p i id_kw f_arg eo)
      | `Kwd ->
         ( Py_params.add_param t `Kwd i id_kw tv opt
         , Py_params.kwd_getter p id_kw f_arg eo)
    in
    ( i+1
    , default
    , (id.name, ast_in)
      ::l
    , t )
  in
  let ty = Py_params.empty in
  let i, def, preamble, ty =
    List.fold_left (load_arg `Pos) (0, [] , []      , ty) args.posonly in
  let i, def, preamble, ty =
    List.fold_left (load_arg `Mix) (i, def, preamble, ty) args.mixed   in
  let _, def, preamble, ty =
    List.fold_left (load_arg `Kwd) (i, def, preamble, ty) args.kwdonly in
  (* dbg_pr "rectype" pp_recty ty; *)
  let sstt_ty = Py_params.build ty in
  (* dbg_pr "sstt_ty" MT.Ty.pp sstt_ty; *)
  let f_type = MlGTy.mk sstt_ty in
  (* dbg_pr "gty" MlGTy.pp f_type; *)
  let f_body = join_let_rev preamble body in
  let f_packed_v = mk_var_t ~kind:MlMVar.Immut ml_fun_packed_name in
  let f_packed = var_of_vart p f_packed_v in
  let f_body = Py_params.(mk_let p [] f_arg_v (unpack p f_packed) f_body) in
  let f_anon = mk_lambda p [] f_type f_packed_v f_body in
  join_let_rev def f_anon

and to_ml (p,e:expr) : MLAst.t = match e with
  | Var v -> var_of_vart p v.name
  | Binop (e1,bop,e2) ->
     mk_2app p (Binop.to_ml p bop) (to_ml e1) (to_ml e2)
  | Cst c -> mk_value p Const.(to_gty c)
  | Lambda (args, _idents, body) ->
    ml_lambda p args (to_ml body)
  | Apply (e,params) ->
     let pos_e = List.map to_ml params.pos in
     let kw_e = List.map (fun (s, e) -> s, to_ml e) params.kwd in
     let args = Py_params.pack p pos_e kw_e in
     mk_app p (to_ml e) args
  | Tuple l -> List.map to_ml l |> mk_tuple p
