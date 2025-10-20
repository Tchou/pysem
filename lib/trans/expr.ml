open Ast

let rec of_expression (env:Env.t) (e:PyreAst.Concrete.Expression.t) : expr =
  let annot = env_annot env in
  match e with
  | BoolOp _ -> failwith "TODO"
  (* | NamedExpr *)
  | BinOp r -> Binop ( of_expression env r.left
                     , Binop.of_binop env r.op
                     , of_expression env r.right) |> annot r.location
  (* | UnaryOp | Lambda | IfExp | Dict | Set | ListComp | SetComp | DictComp
     | GeneratorExp | Await | Yield | YieldFrom | Compare *)
  | Call _ -> failwith "TODO"
  (* | FormattedValue | JoinedStr *)
  | Constant r -> Cst (Const.of_constant env r.value) |> annot r.location
  (* | Attribute | Subscript | Starred *)
  | Name r -> Var (Ident.of_identifier env r.id) |> annot r.location
  (* | List | Tuple | Slice *)

  | NamedExpr _ | UnaryOp _ | Lambda _ | IfExp _ | Dict _ | Set _ | ListComp _
    | SetComp _ | DictComp _ | GeneratorExp _ | Await _ | Yield _ | YieldFrom _
    | Compare _ | FormattedValue _ | JoinedStr _ | Attribute _ | Subscript _
    | Starred _ | List _ | Tuple _ | Slice _
    -> failwith "Not implemented (Expr)."
