open Aliases
open Ast

(** The main purpose of this file is to generate ASTs for test files. The sets
    of identifiers don't need to be correctly built. The randomly generated code
    may crash if the AST built don't take care enough of the expression's
    types. *)

let () = Random.init (1000. *. Sys.time () |> int_of_float)

(* Generation limits *)
let cst_string_limit = 10
and spec_pos_limit = 3
and spec_mix_limit = 3
and spec_kwd_limit = 3
and params_pos_limit = 3
and params_kwd_limit = 3
and instr_block_limit = 2

(* Utils *)

(** [nstring_of_int n i] generate the decimal representation of [i], the string
    should be at least of length [n]. For example [nstring_of_int 5 23] returns
    the string ["00023"] and [nstring_of_int 2 453] returns ["453"]. *)
let nstring_of_int n i =
  let s = string_of_int i in
  let k = n - String.length s in
  if k > 0 then String.init k (fun _ -> '0') ^ s else s
let str3_of_int = nstring_of_int 3

let cpt_p = let n = ref (-1) in fun () -> n := !n+1; !n
let cpt_m = let n = ref (-1) in fun () -> n := !n+1; !n
let cpt_v = let n = ref (-1) in fun () -> n := !n+1; !n
let cpt_k = let n = ref (-1) in fun () -> n := !n+1; !n
let cpt_a = let n = ref (-1) in fun () -> n := !n+1; !n
let cpt_f = let n = ref (-1) in fun () -> n := !n+1; !n
let cpt_x = let n = ref (-1) in fun () -> n := !n+1; !n

(* Generic generators *)

(** Generates a string of given length, with only letters and underscore. The
    first character is a lower letter. *)
let rand_str len =
  String.init len
    ( fun i ->
        let j = if i = 0 then 0 else 1 in
        char_of_int
          (if j * Random.int 53 = 1
           then 95
           else 65 + (Random.int 26) + (j * 32 * Random.int 2)) )

(** If the list is not empty, returns a random item. Calls the second argument
    otherwise. *)
let rand_list_nth l f =
  if l = []
  then f ()
  else List.(nth l (length l |> Random.int))

type id_kind = Ppos | Pmix | Pva | Pkwd | Pvk | Fun | Oth

let kind_of id =
  let s = Ident.show id in
  match String.get s 0 with
  | 'p' -> Ppos
  | 'm' -> Pmix
  | 'v' -> Pva
  | 'k' -> Pkwd
  | 'a' -> Pvk
  | 'f' -> Fun
  | 'x' -> Oth
  | _ -> assert false

let id_is kind id = kind_of id = kind

(* Ast generators *)

let ident_str kind =
  let pref, id = match kind with
    | Ppos -> "p", cpt_p ()
    | Pmix -> "m", cpt_m ()
    | Pva  -> "v", cpt_v ()
    | Pkwd -> "k", cpt_k ()
    | Pvk  -> "a", cpt_a ()
    | Fun  -> "f", cpt_f ()
    | Oth  -> "x", cpt_x ()
  in pref ^ (str3_of_int id)

(** Randomly generates an [ident]. *)
let rec rand_ident () =
  begin match Random.int 7 with
    | 0 -> Ppos
    | 1 -> Pmix
    | 2 -> Pva
    | 3 -> Pkwd
    | 4 -> Pvk
    | 5 -> Fun
    | 6 -> Oth
    | _ -> assert false
  end |> gen_ident

(** Generates an id (named after its kind). *)
and gen_ident kind =
  { name = MlVar.create (Some (ident_str kind))
  ; scope = Parsing.Unknown }
and rand_idents nb =
  List.init nb (fun _ -> rand_ident ())
and gen_idents kind nb =
  List.init nb (fun _ -> gen_ident kind)


(** Generates a random [Ast.const] value. *)
and rand_const () =
  let kind = Random.int 4 in
  match kind with
  | 0 -> None_
  | 1 -> rand_const_bool ()
  | 2 -> rand_const_int ()
  | 3 -> String (Random.int cst_string_limit |> rand_str)
  | _ -> assert false
and rand_const_bool () = Bool (Random.bool ())
and rand_const_int () = Int (Random.int 1000)


(** Generates a random [Ast.binop] value. *)
and rand_binop () =
  let kind = Random.int 16 in
  if kind < 6 (* int -> int *)
  then rand_binop_ii ()
  else if kind < 10
  then rand_binop_ib ()
  else if kind < 12
  then rand_binop_bb ()
  else rand_binop_ab ()
and rand_binop_ii () =
  match Random.int 6 with
  | 0 -> Add
  | 1 -> Sub
  | 2 -> Mult
  | 3 -> Div
  | 4 -> Mod
  | 5 -> Pow
  | _ -> assert false
and rand_binop_ib () =
  match Random.int 4 with
  | 0 -> Lt
  | 1 -> Gt
  | 2 -> Le
  | 3 -> Ge
  | _ -> assert false
and rand_binop_bb () =
  match Random.int 2 with
  | 0 -> Add
  | 1 -> Or
  | _ -> assert false
and rand_binop_ab () = match Random.int 4 with
  | 0 -> Eq
  | 1 -> Neq
  | 2 -> Is
  | 3 -> Isn
  | _ -> assert false

let rand_ident_nth idents = rand_list_nth idents (fun () -> gen_ident Oth)


(** Generates a random [expr] value of [Ast.Var] kind from the given
    identifiers. *)
let rec rand_expr_var idents =
  rand_ident_nth idents |> gen_expr_var
and gen_expr_var v =
  Var v |> dannot
and gen_expr_vars vl =
  List.map gen_expr_var vl

(** Generates a random [expr] value of [Ast.Cst] kind. *)
and rand_expr_cst () = rand_const () |> gen_expr_cst
and rand_expr_cst_bool () = rand_const_bool () |> gen_expr_cst
and rand_expr_cst_int () = rand_const_int () |> gen_expr_cst
and gen_expr_cst cst = Cst cst |> dannot

(** Generates a random [expr] value of [Ast.Binop] kind. *)
and rand_expr_binop () =
  (* 4 kinds: int -> int, int -> bool, bool -> bool, any -> bool *)
  match Random.int 4 with
  | 0 -> rand_expr_binop_ii ()
  | 1 -> rand_expr_binop_ib ()
  | 2 -> rand_expr_binop_bb ()
  | 3 -> rand_expr_binop_ab ()
  | _ -> assert false
and rand_expr_binop_ii () =
  gen_expr_binop
    (rand_expr_cst_int ())
    (rand_binop_ii ())
    (rand_expr_cst_int ())
and rand_expr_binop_ib () =
  gen_expr_binop
    (rand_expr_cst_int ())
    (rand_binop_ib ())
    (rand_expr_cst_int ())
and rand_expr_binop_bb () =
  gen_expr_binop
    (rand_expr_cst_bool ())
    (rand_binop_bb ())
    (rand_expr_cst_bool ())
and rand_expr_binop_ab () =
  gen_expr_binop
    (rand_expr_cst ())
    (rand_binop_ab ())
    (rand_expr_cst ())
and gen_expr_binop e1 bop e2 =
  Binop (e1, bop, e2) |> dannot

(** Generates a [expr] value of [Ast.Tuple] kind with random number of random
    constants. *)
and rand_expr_tuple_cst len =
  List.init len (fun _ -> rand_expr_cst ()) |> gen_expr_tuple
and gen_expr_tuple el =
  Tuple el |> dannot
and gen_expr_tuple_vars vars =
  List.map (fun v -> gen_expr_var v) vars |> gen_expr_tuple

(** Generates a random [expr] of [Ast.Lambda] kind with random parameters and
    body. *)
and rand_expr_lambda () =
  gen_expr_lambda
    (rand_spec ())
    (rand_expr ())
and rand_expr_lambda_simple () = (* 1 mix argument, 1 cst expression *)
  gen_expr_lambda (rand_spec_d 0 1 0 false 0 0 false) (rand_expr_cst ())
and gen_expr_lambda spec ?(sid=Ast.{used=IdentSet.empty; unused=IdentSet.empty}) expr =
  Lambda (spec, sid, expr) |> dannot

(** Generates a random [expr] of [Ast.Apply] kind with random expression and
    parameters. *)
and rand_expr_apply () =
  gen_expr_apply (rand_expr ()) (rand_params ())
and rand_expr_apply_lambda () =
  let f = rand_expr_lambda_simple () in
  gen_expr_apply f (gen_params [] [])
and gen_expr_apply expr params =
  Apply (expr, params) |> dannot

(** Generates a random [expr] value. *)
and rand_expr () : Ast.expr =
  match Random.int 6 with
  | 0 -> rand_expr_var []
  | 1 -> rand_expr_binop ()
  | 2 -> rand_expr_cst ()
  | 3 -> rand_expr_tuple_cst (1 + Random.int 10)
  | 4 -> rand_expr_lambda ()
  | 5 -> rand_expr_apply ()
  | _ -> assert false


(** Generates a random [spec]. The limits for the number of arguments are
    defined at [spec_arg_limit] with "arg" the kind of argument. *)
and rand_spec () =
  let nb_pos = Random.int spec_pos_limit
  and nb_mix = Random.int spec_mix_limit
  and nb_kwd = Random.int spec_kwd_limit in
  rand_spec_d
    nb_pos nb_mix (nb_pos+nb_mix |> Random.int) (Random.bool ())
    nb_kwd (Random.int nb_kwd) (Random.bool ())
and rand_spec_d nb_pos nb_mix nb_pdef hasvarg nb_kwd nb_kdef haskarg =
  let pdefs = List.init nb_pdef (fun _ -> Some (rand_expr_cst ()))
  and kdefs = List.init nb_kdef (fun _ -> Some (rand_expr_cst ())) in
  gen_spec nb_pos nb_mix pdefs hasvarg nb_kwd kdefs haskarg

(** [gen_spec nb_pos nb_mix nb_pdef hasvarg nb_kwd nb_kdef haskarg] generates a
    spec with specified number of positional, mixed and keyword arguments, and
    defult arguments. Must not have more than pos + mix default positionals,
    same for keyword defaults. *)
and gen_spec nb_po nb_mx pdefs hasvarg nb_kw kdefs haskarg =
  let nb_pdef, nb_kdef = List.(length pdefs, length kdefs) in
  if nb_pdef > nb_po + nb_mx || nb_kdef > nb_kw
  then Invalid_argument "Too many default arguments." |> raise
  else if Int.(min nb_po nb_mx |> min nb_kw |> min nb_pdef |> min nb_kdef) < 0
  then Invalid_argument "Negative argument(s)." |> raise;
  let pndef, pdef, mndef=
    if nb_pdef <= nb_mx
    then nb_po, 0, nb_mx - nb_pdef
    else nb_po - (nb_pdef-nb_mx), nb_pdef-nb_mx, 0
  in
  let podefs, mxdefs =
    List.( init pndef (fun _ -> None) @ take pdef pdefs
         , init mndef (fun _ -> None) @ drop pdef pdefs)
  in
  { posonly =
      List.(combine
              (init nb_po (fun _ -> gen_ident Ppos))
              podefs)
  ; mixed   =
      List.(combine
              (init nb_mx (fun _ -> gen_ident Pmix))
              mxdefs)
  ; vararg  = if hasvarg then Some (gen_ident Pva) else None
  ; kwdonly =
      List.(combine
              (init nb_kw (fun _ -> gen_ident Pkwd))
              (init (nb_kw-nb_kdef) (fun _ -> None) @ kdefs))
  ; kwdarg  = if haskarg then Some (gen_ident Pvk) else None }


(** Generates a [params] value with random [expr] values. *)
and rand_params () =
  rand_params_len (Random.int params_pos_limit) (Random.int params_kwd_limit)
and rand_params_len nb_pos nb_kwd =
  gen_params
    (List.init nb_pos (fun _ -> rand_expr ()))
    List.(init nb_kwd (fun _ -> ident_str Pkwd)
          |> map (fun k -> (k,rand_expr ())))
and gen_params po_val kw_val =
  { pos = po_val
  ; kwd = kw_val }

(** Extract all identifiers of the given [spec] argument. *)
let idents_of_spec {posonly; mixed; vararg; kwdonly; kwdarg} =
  let extract_il = List.map (fun (i,_) -> i)
  and extract_io = Option.to_list in
  extract_il posonly
  @ extract_il mixed
  @ extract_io vararg
  @ extract_il kwdonly
  @ extract_io kwdarg
and kwds_of_params {kwd;_} = List.map (fun (k,_) -> k) kwd


(** Generates an [instr] value of kind [Ast.Block]. A default length limit is
    defined at [instr_block_limit]. *)
let rec rand_instr_block () =
  Random.int instr_block_limit |> rand_instr_block_len
and rand_instr_block_len size =
  List.init size
    (fun _ -> rand_instr ())
  |> gen_instr_block
and rand_instr_block_iexpr () =
  Random.int instr_block_limit |> rand_instr_block_iexpr_len
and rand_instr_block_iexpr_len size =
  List.init size
    (fun _ -> rand_instr_iexpr ())
  |> gen_instr_block
and gen_instr_block il =
  Block il |> dannot

(** Generates an [instr] value of kind [Ast.FunDef]. *)
and rand_instr_fundef () =
  let spec = rand_spec ()
  and body = rand_instr () in
  gen_instr_fundef_ spec body
and rand_instr_fundef_bodylen size =
  let spec = rand_spec ()
  and body = rand_instr_block_len size in
  gen_instr_fundef_ spec body
and gen_instr_fundef_ spec body =
  gen_instr_fundef (gen_ident Fun) spec body
and gen_instr_fundef f spec ?(sid=Ast.{used=IdentSet.empty; unused=IdentSet.empty}) body =
  FunDef (f, spec, sid, body) |> dannot

(** Generates an [instr] value of kind [Ast.Return]. *)
and rand_instr_return () =
  gen_instr_return (if Random.bool () then Some (rand_expr ()) else None)
and gen_instr_return_ () =
  gen_instr_return None
and gen_instr_return expr =
  Return expr |> dannot

(** Generates an [instr] value of kind [Ast.Assign]. *)
and rand_instr_assign () =
  gen_instr_assign
    (rand_ident ())
    (rand_expr ())
and rand_instr_assign_select idents =
  gen_instr_assign
    (rand_ident_nth idents)
    (rand_expr ())
and rand_instr_assign_cst target =
  gen_instr_assign target (rand_expr_cst ())
and gen_instr_assign target expr =
  Assign (target, expr) |> dannot

(** Generates an [instr] value of kind [Ast.Iexpr]. *)
and rand_instr_iexpr () =
  rand_expr () |> gen_instr_iexpr
and rand_instr_iexpr_cst () =
  rand_expr_cst () |> gen_instr_iexpr
and gen_instr_iexpr e =
  Iexpr e |> dannot

(** Generates an [instr] value of kind [Ast.break]. *)
and gen_instr_break () = Break |> dannot

(** Generates a random [instr] value. *)
and rand_instr () =
  match Random.int 6 with
  | 0 -> rand_instr_block ()
  | 1 -> rand_instr_fundef ()
  | 2 -> rand_instr_return ()
  | 3 -> rand_instr_assign ()
  | 4 -> rand_instr_iexpr ()
  | 5 -> gen_instr_break ()
  | _ -> assert false


let rec get_ids (_,instr) = match instr with
  | Block is -> List.(map get_ids is |> flatten)
  | FunDef (fid,_,_,_) -> [fid]
  | Assign (tg,_) -> [tg]
  | _ -> []

let rand_prog_len size =
  let rec fold n instrl =
    if n <= 0
    then instrl
    else
      let i = rand_instr () in
      fold (n-1) (i::instrl)
  in
  fold size [] |> List.rev

(* Generate test ASTs *)

let decr (a,b,c) =
  (* kind of lexicographical order
     .     b<=0        b>0
     a<=0  bot         a,b-1,c+1
     a >0  a-1,b+1+c,0 a,b-1,c+1
  *)
  if b > 0
  then a,b-1,c+1
  else if a > 0
  then a-1,b+1+c,0
  else Invalid_argument "Triplet already bot!" |> raise
and is_bot (a,b,_) = a <= 0 && b <= 0

(** Generate function definition for all specs of [n] arguments. *)
let all_fundef ?(defaults=true) i =
  let rec fold_spec acc ((pos,mix,kwd) as t) =
    (* (pos+mix) * kwd = po * kw specs : for each number of default po
       arguments there can be kw default kw arguments. *)
    let po = pos + mix in
    let rec fold_spec_default dacc (p,k) =
      let dacc = rand_spec_d pos mix p false kwd k false :: dacc in
      if k >= kwd
      then if p >= po
        then dacc
        else fold_spec_default dacc (p+1,0)
      else fold_spec_default dacc (p,k+1)
    in
    let specs =
      if defaults
      then fold_spec_default [] (0,0)
      else [rand_spec_d pos mix 0 false kwd 0 false] in
    if is_bot t
    then specs @ acc
    else fold_spec (specs @ acc) (decr t)
  in
  fold_spec [] (i,0,0)
  |> List.rev
  |> List.map (fun spec ->
      idents_of_spec spec
      |> gen_expr_tuple_vars
      |> gen_instr_iexpr
      |> gen_instr_fundef_ spec)

(** Generate function definition for all specs from [0] to [n] arguments ([n]
    included). *)
let rec all_fundef_till n =
  if n < 0
  then []
  else let fbeg = all_fundef_till (n-1)
    and fend = all_fundef n in
    fbeg @ fend
