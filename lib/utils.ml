module Aliases = struct
  module PC = PyreAst.Concrete
  module PCI = PC.Identifier

  module MC = Mlsem.Common
  module MS = Mlsem.System
  module ML = Mlsem.Lang
  module MT = Mlsem.Types
  module MlVar = Mlsem.Common.Variable
  module MlMVar = Mlsem.Lang.MVariable
  module MlAst = Mlsem.Lang.Ast
  module MSAst = Mlsem.System.Ast
  module MlGTy = Mlsem.Types.GTy
end

open Aliases


(* DEBUG *)

let debug = ref true
and export = ref true

let pr str pp_t t =
  Format.printf "\027[1m%s:\027[0m@.@[%a@]@.--@\n%!" str pp_t t
let dbg_pr str pp_t t =
  if !debug
  then pr str pp_t t


(* STRINGS *)

let mk_id fmt =
  Format.kasprintf (fun s -> (if !export then "" else "%") ^ s) fmt

let ml_fun_arg_name = mk_id "%s" "rec_arg"
let ml_fun_darg_name = mk_id "%s" "def_arg"
let field_name_pos i = mk_id "p_%d" i
and field_name_arg i k = mk_id "a_%d_%s" i k
and field_name_kw  k = mk_id "k_%s" k
and def_var_name k = mk_id "d_%s" k

let getter_pk_name field = mk_id "get_%s_?" field
and getter_a_name i k d =
  mk_id "get_%d_%s%s" i k (if d then "_?" else "")


(* MAKE TYPES *)

let mk_tv ?(k=MT.TVar.KInfer) str =
  MT.TVar.(mk k (Some str) |> typ)
let mk_rec_disj opn (fbt_ll:(string * (bool * MT.Ty.t)) list list) =
  let open MT in
  List.map (MT.Record.mk opn) fbt_ll
  |> Ty.disj

let ty_of_int i =
  let z = Z.of_int i in
  MT.Ty.interval (Some z) (Some z)


(* MAKE System.Ast *)

let ml_annot p (ast:MlAst.e) =
  (MC.Eid.unique_with_pos p, ast)

let mk_value pos gty = MlAst.Value gty |> ml_annot pos
let mk_unit pos = mk_value pos (MlGTy.mk MT.Ty.unit)

let mk_var_t ?(kind=MlMVar.Mut) str = MlMVar.create kind (Some str)
let mk_var pos ?(kind=MlMVar.Mut) str =
  MlAst.Var (mk_var_t ~kind str) |> ml_annot pos
and var_of_vart pos v = Var v |> ml_annot pos

let mk_record pos decl_l expr_l =
  MlAst.Constructor
    ( MSAst.Rec (List.map (fun dec -> dec, false) decl_l, false)
    , expr_l) |> ml_annot pos

let mk_lambda pos ty gty id body =
  MlAst.Lambda (ty, gty, id, body) |> ml_annot pos

let mk_ite pos test ty thn els = MlAst.Ite (test,ty,thn,els) |> ml_annot pos

let mk_app pos f x = MlAst.App (f,x) |> ml_annot pos
let mk_2app pos f x y = mk_app pos (mk_app pos f x) y

let mk_projection pos proj ast = MlAst.Projection (proj, ast) |> ml_annot pos

let mk_let pos ty id ast_in ast_out =
  MlAst.Let (ty,id,ast_in,ast_out) |> ml_annot pos

let mk_seq pos a1 a2 = MlAst.Seq (a1, a2) |> ml_annot pos

let mk_return pos ast = MlAst.Return ast |> ml_annot pos

let mk_break pos = MlAst.Break |> ml_annot pos


(* AST UTILS *)

let dummy_pos = MC.Position.dummy
let dummy_ml_ast = mk_value dummy_pos MlGTy.any
let dummy_var_t = mk_var_t "_"

let mk_ite_rectest pos record field opn thn els =
  mk_ite pos record
    MT.(Record.mk opn
          [ field, (false,MT.Ty.any) ])
    thn els

let mk_getter_pk field =
  let tv = mk_tv field in
  let gty = mk_rec_disj true [[ (field, (true, tv)) ]] |> MlGTy.mk in
  let f_arg_v = mk_var_t ~kind:MlMVar.Immut ml_fun_arg_name in
  let f_arg = var_of_vart dummy_pos f_arg_v in
  let f_darg_v = mk_var_t ~kind:MlMVar.Immut ml_fun_darg_name in
  let f_darg = var_of_vart dummy_pos f_darg_v in
  mk_ite_rectest dummy_pos f_arg field true
    (mk_projection dummy_pos (MSAst.Field field) f_arg)
    (f_darg)
  |> mk_lambda dummy_pos [] (mk_tv ml_fun_darg_name |> MlGTy.mk) f_darg_v
  |> mk_lambda dummy_pos [] gty f_arg_v

let mk_getter_a f_p f_k f_a d =
  let tv = mk_tv f_a in
  let gty = mk_rec_disj true [[(f_p, (d, tv))]; [(f_k, (d, tv))]] |> MlGTy.mk in
  let f_arg_v = mk_var_t ~kind:MlMVar.Immut ml_fun_arg_name in
  let f_arg = var_of_vart dummy_pos f_arg_v in
  let proj_p = mk_projection dummy_pos (MSAst.Field f_p) f_arg in
  let proj_k = mk_projection dummy_pos (MSAst.Field f_k) f_arg in
  let body =
    if d
    then let f_darg_v = mk_var_t ~kind:MlMVar.Immut ml_fun_darg_name in
         let f_darg = var_of_vart dummy_pos f_darg_v in
         mk_ite_rectest dummy_pos f_arg f_p true
           proj_p
           (mk_ite_rectest dummy_pos f_arg f_k true
              proj_k f_darg)
         |> mk_lambda dummy_pos [] (mk_tv ml_fun_darg_name |> MlGTy.mk) f_darg_v
    else mk_ite_rectest dummy_pos f_arg f_p true
           proj_p proj_k in
  mk_lambda dummy_pos [] gty f_arg_v body


let mlvar_get_name v = match MlVar.get_name v with
  | Some str -> str
  | None -> failwith "MlVar with no name?"

let count_if p l =
  List.fold_left (fun acc e -> if p e then acc + 1 else acc) 0 l


(* PYRE UTILS *)

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
