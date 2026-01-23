open Aliases

type res_kind = R | V

type expr' =
  | Const of Ast.const
  | Var of Ast.ident
  | Res of res_kind * expr
  | Proj of res_kind * expr
  | IfNotRes of {cond:expr; v:MlVar.t; s:MlVar.t; body:expr}
  | Ite of expr * expr * expr
  | Tuple of expr list
  | Pi of int * expr
  | EmptyRec
  | RecUpdate of expr * Ast.ident * expr
  | Field of expr * Ast.ident
  | Lambda of Ast.ident * expr
  | LambdaAdm of MlVar.t * expr
  | App of expr * expr
and expr = MC.Position.t * expr'

let mkvar () = MlVar.create None
let var v = Ast.{name=v; scope=Parsing.Local}

(*
  IfNotRes e1 e2
  <=>
  match e1 with
  | Tuple [Res (R, v); s] as r -> r
  | Tuple [Res (V, v); s] -> e2 v s
 *)

let subst _e1 _x _e2 = assert false
(* e1 [x ← e2] *)

let rec reduce (p,e) = p, match e with
  | Const _ | Var _ | EmptyRec -> e
  | Res (k,e) -> Res (k, reduce e)
  | Proj (k,e) -> (match reduce e with
                   | _p', Res (k', (_,e')) when k = k' -> e'
                   | e -> Proj (k,e))
  | IfNotRes r -> (
    let _pc, cond = reduce r.cond in
    let _pb, _body = reduce r.body in
    match cond with
    | Res (R,_e) -> cond
    | Res (V,_e) -> subst r.body r.v ()
    | _ -> failwith "TODO"
  )
  | _ -> failwith "TODO"
