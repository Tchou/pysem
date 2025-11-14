module Aliases = struct
  module PC = PyreAst.Concrete
  module PCI = PC.Identifier

  module MC = Mlsem.Common
  module MlVar = Mlsem.Common.Variable
  module MlMVar = Mlsem_lang.MVariable
  module MlAst = Mlsem_lang.Ast
  module MSAst = Mlsem_system.Ast
  module MlT = Mlsem.Types
  module MlGTy = Mlsem.Types.GTy
end

open Aliases

let debug = ref true
let dbg_pr str pp_t t =
  if !debug
  then Format.printf ">> %s:@.  @[%a@]@." str pp_t t

let mk_id fmt = Format.kasprintf (fun s -> "%" ^ s) fmt

let ml_annot p (ast:MlAst.e) =
  (MC.Eid.unique_with_pos p, ast)

let mk_tv ?(k=MlT.TVar.KInfer) str =
  MlT.TVar.(mk k (Some str) |> typ)
let mk_rec_disj opn (fbt_ll:(string * (bool * MlT.Ty.t)) list list) =
  let open MlT in
  List.map (MlT.Record.mk opn) fbt_ll
  |> Ty.disj

let mk_var_t ?(kind=MlMVar.Mut) str =
  MlMVar.create kind (Some str)

let mk_var pos ?(kind=MlMVar.Mut) str =
  MlAst.Var (mk_var_t ~kind str) |> ml_annot pos
and var_of_vart pos v = Var v |> ml_annot pos
let mk_record pos decl_l expr_l =
  MlAst.Constructor
    ( MSAst.Rec (List.map (fun dec -> dec, false) decl_l, false)
    , expr_l) |> ml_annot pos
let mk_projection pos proj ast =
  MlAst.Projection (proj, ast) |> ml_annot pos
let mk_lambda pos ty gty id body =
  MlAst.Lambda (ty, gty, id, body) |> ml_annot pos
let mk_let pos ty id ast_in ast_out =
  MlAst.Let (ty,id,ast_in,ast_out) |> ml_annot pos
let mk_ite pos test ty thn els =
  MlAst.Ite (test,ty,thn,els) |> ml_annot pos

let arg_name_pos i = mk_id "p_%d" i
and arg_name_arg a = mk_id "a_%s" a
and arg_name_kw  k = mk_id "k_%s" k
and def_arg_name k = mk_id "d_%s" k

let mlvar_get_name v = match MlVar.get_name v with
  | Some str -> str
  | None -> failwith "MlVar with no name?"

let ty_of_int i =
  let z = Z.of_int i in
  MlT.Ty.interval (Some z) (Some z)

let count_if p l =
  List.fold_left (fun acc e -> if p e then acc + 1 else acc) 0 l

let pos_converter filename text =
  let[@tail_mod_cons] rec loop i len =
    if i >= len then []
    else if text.[i] = '\n' then (i+1)::(loop (i+1) len)
    else loop (i+1) len
  in
  let bol = (0 :: loop 0 (String.length text)) |> Array.of_list in
  fun (l : PC.Position.t) ->
  Lexing.{ pos_fname = filename
         ; pos_lnum = l.line
         ; pos_bol = bol.(l.line-1)
         ; pos_cnum = l.column + bol.(l.line-1) }

type loc_converter = PC.Location.t -> MC.Position.t
let loc_converter filename text =
  let to_pos = pos_converter filename text in
  fun (p: PC.Location.t) ->
    let l1 = to_pos p.start in
    let l2 = to_pos p.stop in
    MC.Position.(with_poss l1 l2 () |> position)

let pp_py_expr fmt e =
  Format.fprintf fmt "%a"
    (Sexplib0.Sexp.pp) (PC.Expression.sexp_of_t e)
