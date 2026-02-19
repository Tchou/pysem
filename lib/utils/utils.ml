open Aliases

(* VARIABLES *)

let debug = ref false (** Show debug informations. *)

and export = ref false (** Print code with legals characters. *)

(* let shadowing = ref false (\** Allow shadowing in toplevel. *\) *)

let user_vars =
  [ debug , "PYSEM_DEBUG"
  ; export, "PYSEM_EXPORT"
  ] (** Configurable variables. *)

let sh_values = [ "true",true ]

(* STRINGS *)

let internal_prefix = "%"
let is_internal = String.starts_with ~prefix:internal_prefix
let internal s = internal_prefix ^ s
let strip_internal s =
  assert (is_internal s);
  String.sub s 2 (String.length s - 2)

let mk_internal fmt = Format.kasprintf internal fmt
let mk_id fmt = Format.kasprintf (fun s -> if !export then s
                                   else internal s) fmt
let ml_fun_arg_name = mk_id "fun_arg"
let ml_fun_packed_name = mk_id "fun_packed"
let ml_fun_darg_name = mk_id "def_arg"
let def_var_name k = mk_id "d_%s" k

let gen_cpt () =
  let cpt = ref (-1) in
  let get () = cpt :=!cpt+1 ; !cpt in
  let str_get () = get () |> string_of_int in
  (get,str_get)

(* MAKE TYPES *)

let mk_tv ?(k=MT.KInfer) str =
  MT.TVar.(mk k (Some str) |> typ)
let mk_rec_disj opn (fbt_ll:(string * (MT.Ty.t*bool)) list list) =
  let open MT in
  List.map MT.Record.(if opn then mk_open else mk_closed) fbt_ll
  |> Ty.disj

let ty_of_int i =
  let z = Z.of_int i in
  MT.Ty.interval (Some z) (Some z)

(* MAKE Lang.Ast *)

let ml_annot p (ast:MLAst.e) =
  (MC.Eid.unique_with_pos p, ast)

let mk_value pos gty = MLAst.Value gty |> ml_annot pos

let mk_var_t ?(kind=MlMVar.Mut) str = MlMVar.create kind (Some str)
let mk_var pos ?(kind=MlMVar.Mut) str =
  MLAst.Var (mk_var_t ~kind str) |> ml_annot pos
and var_of_vart pos v = Var v |> ml_annot pos

let mk_enum pos e =
  MLAst.Constructor (MSAst.Enum e, []) |> ml_annot pos
let mk_tag pos t e =
  MLAst.Constructor (MSAst.Tag t, [e]) |> ml_annot pos

let mk_proj_tag pos t e =
  MLAst.Projection (MSAst.PiTag t, e) |> ml_annot pos
let mk_proj_tuple pos n i e =
  MLAst.Projection (MSAst.Pi (n, i), e) |> ml_annot pos

let mk_tuple pos l =
  MLAst.Constructor (MSAst.Tuple (List.length l), l) |> ml_annot pos
let mk_record pos decl_l expr_l =
  MLAst.Constructor
    ( MSAst.Rec (decl_l, false)
    , expr_l) |> ml_annot pos

let mk_record_update pos id expr record =
  let open MS.Ast in
  MLAst.Operation
    ( RecUpd id
    , (Constructor (Tuple 2, [record; expr]))
      |> ml_annot pos )
  |> ml_annot pos

let mk_lambda pos ty gty id body =
  MLAst.Lambda (ty, gty, id, body) |> ml_annot pos

let mk_ite pos test ty thn els = MLAst.Ite (test,ty,thn,els) |> ml_annot pos

let mk_ite_approx pos cond gty then_ else_ =
  let ty = MlGTy.ub gty in
  MLAst.Constructor ((MSAst.Ternary ty) ,[cond;then_;else_]) |> ml_annot pos

let mk_app pos f x = MLAst.App (f,x) |> ml_annot pos

let mk_projection pos proj ast = MLAst.Projection (proj, ast) |> ml_annot pos

let mk_let pos ty id ast_in ast_out =
  MLAst.Let (ty,id,ast_in,ast_out) |> ml_annot pos

let mk_varassign pos v ast = MLAst.VarAssign (v, ast) |> ml_annot pos

let mk_seq pos a1 a2 = MLAst.Seq (a1, a2) |> ml_annot pos

let mk_return pos ast = MLAst.Return ast |> ml_annot pos

let mk_break pos = MLAst.Break |> ml_annot pos

let mk_rec_del pos id e =
  MLAst.(Operation(SA.RecDel id, e)) |> ml_annot pos

(* AST UTILS *)

let dummy_pos = MC.Position.dummy
let dummy_ml_ast = mk_value dummy_pos MlGTy.any
let dummy_var = "_"
let dummy_var_t () = mk_var_t dummy_var

let mk_unit pos = mk_value pos (MlGTy.mk MT.Ty.unit)

let mk_ite_rectest pos record field _opn thn els =
  mk_ite pos record
    MT.(Record.mk_open
          [ field, (MT.Ty.any, false) ]|> GTy.mk)
    thn els

let mk_2app pos f x y = mk_app pos (mk_app pos f x) y

let join_let_rev var_in last =
  List.fold_left (fun body (v,e) ->
      mk_let (fst e |> MC.Eid.loc) [] v e body) last var_in

let count_if p l =
  List.fold_left (fun acc e -> if p e then acc + 1 else acc) 0 l

(* PYRE UTILS *)

let pos_converter filename text =
  let[@tail_mod_cons] rec loop i len =
    if i >= len
    then []
    else if text.[i] = '\n'
    then (i+1)::(loop (i+1) len)
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
  Format.fprintf fmt "%a" Sexplib0.Sexp.pp (PC.Expression.sexp_of_t e)
