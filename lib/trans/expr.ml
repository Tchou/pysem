open Ast

let of_expression (env:Env.t) (e:PyreAst.Concrete.Expression.t) : expr =
  let annot = env_annot env in
  match e with
  | Call _ -> failwith "TODO"
  | Constant r -> Cst (Const.of_constant env r.value) |> annot r.location
  | Name r -> Var (Ident.of_identifier env r.id) |> annot r.location
  | _ -> failwith "Not implemented (Expr)."
