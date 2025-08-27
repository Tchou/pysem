let mk_id fmt = Format.kasprintf (fun s -> "%" ^ s) fmt

let ty_cst_int i =
  let z = Z.of_int i in
  Mlsem.Types.Ty.interval (Some z) (Some z) 

let count_if p l = List.fold_left (fun acc e -> if p e then acc + 1 else acc) 0 l

let pp_expr fmt e =
  Format.fprintf fmt "%a" (Sexplib0.Sexp.pp) (PyreAst.Concrete.Expression.sexp_of_t e)



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
