let mk_id fmt = Format.kasprintf (fun s -> "%" ^ s) fmt

let ml_annot p (ast:Mlsem_lang.Ast.e) =
  (Mlsem.Common.Eid.unique_with_pos p, ast)

let mk_var_t ?(kind=Mlsem_lang.MVariable.Mut) str =
  Mlsem_lang.MVariable.create kind (Some str)

let mk_var pos ?(kind=Mlsem_lang.MVariable.Mut) str =
  Mlsem_lang.Ast.Var (mk_var_t ~kind str) |> ml_annot pos
let var_of_vart pos v = Var v |> ml_annot pos
let mk_projection pos proj ast =
  Mlsem_lang.Ast.Projection (proj, ast) |> ml_annot pos
let mk_lambda pos ty ?(gty=Mlsem.Types.GTy.any) id body =
  Mlsem_lang.Ast.Lambda (ty, gty, id, body) |> ml_annot pos
let mk_let pos ?(ty=[]) id ast_in ast_out =
  Mlsem_lang.Ast.Let (ty,id,ast_in,ast_out) |> ml_annot pos
let mk_ite pos test ty thn els =
  Mlsem_lang.Ast.Ite (test,ty,thn,els) |> ml_annot pos

let arg_name_pos i = mk_id "p_%d" i
and arg_name_kw  k = mk_id "k_%s" k
and def_arg_name k = mk_id "d_%s" k

let ty_of_int i =
  let z = Z.of_int i in
  Mlsem.Types.Ty.interval (Some z) (Some z) 

let count_if p l =
  List.fold_left (fun acc e -> if p e then acc + 1 else acc) 0 l

let pos_converter filename text =
  let[@tail_mod_cons] rec loop i len =
    if i >= len then []
    else if text.[i] = '\n' then (i+1)::(loop (i+1) len)
    else loop (i+1) len
  in
  let bol = (0 :: loop 0 (String.length text)) |> Array.of_list in
  fun (l : PyreAst.Concrete.Position.t) ->
    Lexing.{ pos_fname = filename; pos_lnum = l.line;
             pos_bol = bol.(l.line-1);
             pos_cnum = l.column + bol.(l.line-1) }

type loc_converter = PyreAst.Concrete.Location.t -> Mlsem.Common.Position.t
let loc_converter filename text =
  let to_pos = pos_converter filename text in
  fun (p: PyreAst.Concrete.Location.t) ->
    let l1 = to_pos p.start in
    let l2 = to_pos p.stop in
    Mlsem.Common.Position.(with_poss l1 l2 () |> position)

let pp_py_expr fmt e =
  Format.fprintf fmt "%a"
    (Sexplib0.Sexp.pp) (PyreAst.Concrete.Expression.sexp_of_t e)
