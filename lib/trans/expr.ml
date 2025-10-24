open Ast

let zip_for l1 l2 =
  let rec loop l1 l2 acc =
    match l1, l2 with
    | [], l2 -> acc, l2
    | e1 :: ll1, e2 :: ll2 -> loop ll1 ll2 ((e1, Some e2)::acc)
    | e1 :: ll1, []        -> loop ll1 []  ((e1, None   )::acc)
  in
  loop l1 l2 []

let rec of_expression (env:Env.t) (e:PyreAst.Concrete.Expression.t) : expr =
  let annot = env_annot env in
  match e with
  | BoolOp ({values=[a;b];_} as r) ->
     Binop ( of_expression env a
           , Binop.of_boolop env r.op
           , of_expression env b) |> annot r.location
  | BoolOp _ -> failwith "Not implemented (Expr.BoolOp(values<>[a;b]))."
  (* | NamedExpr *)
  | BinOp r -> Binop ( of_expression env r.left
                     , Binop.of_binop env r.op
                     , of_expression env r.right) |> annot r.location
  (* | UnaryOp *)
  | Lambda r ->
     let lenv =
       let open Parsing in
       let bi = BlockId.mk_lambda r.location in
       { env with current = BidTable.find env.infos bi } in
     Lambda ( spec_of_arguments lenv r.args
            , of_expression lenv r.body ) |> annot r.location
  (* | IfExp | Dict | Set | ListComp | SetComp | DictComp | GeneratorExp | Await
     | Yield | YieldFrom *)
  | Compare ({ops=[op];comparators=[right];_} as r) ->
     Binop ( of_expression env r.left
           , Binop.of_comparisonoperator env op
           , of_expression env right) |> annot r.location
  | Compare _ -> failwith "Not implemented (Expr.Compare(¬ only 2 arguments))."
  | Call ({func=Name _;_} as r) ->
     let kw =
       List.map
         (fun kw ->
           let open PC.Keyword in
           match kw.arg with
           | Some id -> ( Ident.of_identifier env id
                        , of_expression env kw.value )
           | None -> failwith "Not implemented (Expr.Call(several kw_args)).")
         r.keywords in
     Apply ( of_expression env r.func
           , { pos = List.map (of_expression env) r.args
             ; kw } )
     |> annot r.location
  | Call _ -> failwith "Not implemented (Expr.Call(complex expression))."
  (* | FormattedValue | JoinedStr *)
  | Constant r -> Cst (Const.of_constant env r.value) |> annot r.location
  (* | Attribute | Subscript | Starred *)
  | Name r -> Var (Ident.of_identifier env r.id) |> annot r.location
  (* | List | Tuple | Slice *)

  | NamedExpr _ | UnaryOp _ | IfExp _ | Dict _ | Set _ | ListComp _ | SetComp _
    | DictComp _ | GeneratorExp _ | Await _ | Yield _ | YieldFrom _
    | FormattedValue _ | JoinedStr _ | Attribute _ | Subscript _ | Starred _
    | List _ | Tuple _ | Slice _
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

module MC = Mlsem.Common
module MlAst = Mlsem_lang.Ast

let ml_annot p (ast:MlAst.e) = (MC.Eid.unique_with_pos p, ast)

let to_ml (p,e:expr) : MlAst.t = match e with
  | Var _ -> failwith "TODO"
  | Binop _ -> failwith "TOOD"
  | Cst c -> Value Const.(to_gty c) |> ml_annot p
  | Lambda _ -> failwith "TOOD"
  | Apply _ -> failwith "TOOD"
