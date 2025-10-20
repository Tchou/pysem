open Ast

let rec of_expression (env:Env.t) (e:PyreAst.Concrete.Expression.t) : expr =
  let annot = env_annot env in
  match e with
  | BinOp r -> Binop ( of_expression env r.left
                     , Binop.of_binop env r.op
                     , of_expression env r.right) |> annot r.location
  | Call _ -> failwith "TODO"
  | Constant r -> Cst (Const.of_constant env r.value) |> annot r.location
  | Name r -> Var (Ident.of_identifier env r.id) |> annot r.location
  | _ -> failwith "Not implemented (Expr)."
