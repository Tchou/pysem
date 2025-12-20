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
    let lenv = Parsing.BlockId.mk_lambda r.location
               |> Env.upd env in
    Lambda ( spec_of_arguments lenv r.args
           , of_expression lenv r.body ) |> annot r.location
  (* | IfExp | Dict | Set | ListComp | SetComp | DictComp | GeneratorExp | Await
     | Yield | YieldFrom *)
  | Compare ({ops=[op];comparators=[right];_} as r) ->
    Binop ( of_expression env r.left
          , Binop.of_comparisonoperator op
          , of_expression env right) |> annot r.location
  | Compare _ -> failwith "Not implemented (Expr.Compare(¬ only 2 arguments))."
  | Call ({func=Name _;_} as r) ->
    let kw =
      List.map
        (fun kw ->
           let open PC.Keyword in
           match kw.arg with
           | Some id -> ( PCI.to_string id
                        , of_expression env kw.value )
           | None -> failwith "Not implemented (Expr.Call(several kw_args)).")
        r.keywords in
    Apply ( of_expression env r.func
          , { pos = List.map (of_expression env) r.args
            ; kw } )
    |> annot r.location
  | Call _ -> failwith "Not implemented (Expr.Call(complex expression))."
  (* | FormattedValue | JoinedStr *)
  | Constant r -> Cst (Const.of_constant r.value) |> annot r.location
  (* | Attribute *)
  (* | Subscript r ->
     Projection (of_expression env r.slice, of_expression env r.value) |> annot r.location *)
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
  let args, rem_args = zip_for (List.rev a.args) (List.rev a.defaults) in
  let posonly, rem_pos = zip_for (List.rev a.posonlyargs) rem_args in
  assert (rem_pos = []);
  let kwonly = List.combine a.kwonlyargs a.kw_defaults in
  let format (arg,eo) = Ident.of_argument fenv arg
                      , Option.map (of_expression fenv) eo in
  { posonly = List.map format posonly
  ; args    = List.map format args
  ; kwonly  = List.map format kwonly
  ; vararg  = Option.map (Ident.of_argument fenv) a.vararg
  ; kwarg   = Option.map (Ident.of_argument fenv) a.kwarg }

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
  let get_pos i = mk_projection p (MSAst.Field (field_name_pos i)) f_arg in
  let get_kw id = mk_projection p (MSAst.Field (field_name_kw id)) f_arg in

  let load_arg (pak:[`Pos|`Arg|`Kwd]) (i,d,l,t) (id,eo) =
    let opt, eo, default = match eo with
      | None -> false, None, d
      | Some e ->
        let e = mk_var_t ~kind:MlMVar.Immut
            (Ident.show id |> def_var_name)
              , to_ml e in
        true, Some e, e::d in
    let id_kw = Ident.show id in
    let t, ast_in = match pak with
      | `Pos ->
        let field = field_name_pos i in
        let tv = field |> mk_tv in
        ( Py_params.add_param t `Pos i id_kw tv opt
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
        ( Py_params.add_param t `Arg i id_kw tv opt
        , get )
      | `Kwd ->
        let field = field_name_kw id_kw in
        let tv = field |> mk_tv in
        ( Py_params.add_param t `Kwd i id_kw tv opt
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
  let ty = Py_params.empty in
  let i, def, preamble, ty =
    List.fold_left (load_arg `Pos) (0, [] , []      , ty) args.posonly in
  let i, def, preamble, ty =
    List.fold_left (load_arg `Arg) (i, def, preamble, ty) args.args    in
  let _, def, preamble, ty =
    List.fold_left (load_arg `Kwd) (i, def, preamble, ty) args.kwonly  in
  (* dbg_pr "rectype" pp_recty ty; *)
  let sstt_ty = Py_params.build ty in
  (* dbg_pr "sstt_ty" MT.Ty.pp sstt_ty; *)
  let f_type = MlGTy.mk sstt_ty in
  (* dbg_pr "gty" MlGTy.pp f_type; *)
  let f_body = join_let_rev preamble body in
  let f_packed_v = mk_var_t ~kind:MlMVar.Immut ml_fun_packed_name in
  let f_packed = var_of_vart p f_packed_v in
  let f_body = Py_params.(mk_let p [extract_record sstt_ty] f_arg_v (unpack p f_packed) f_body) in
  let f_anon = mk_lambda p [] f_type f_packed_v f_body in
  join_let_rev def f_anon

and to_ml (p,e:expr) : MLAst.t = match e with
  | Var v -> var_of_vart p v.name
  | Binop (e1,bop,e2) ->
    mk_2app p (Binop.to_ml p bop) (to_ml e1) (to_ml e2)
  | Cst c -> mk_value p Const.(to_gty c)
  | Lambda (args, body) ->
    ml_lambda p args (to_ml body)
  | Apply (e,params) ->
    let pos_e = List.map to_ml params.pos in
    let kw_e = List.map (fun (s, e) -> s, to_ml e) params.kw in
    let args = Py_params.pack p pos_e kw_e in
    mk_app p (to_ml e) args
  | Tuple l -> List.map to_ml l |> mk_tuple p
