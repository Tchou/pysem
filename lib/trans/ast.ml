open Aliases

type 'a annot = MC.Position.t * 'a

type ident =
  { name : MlVar.t
  ; scope : Parsing.scope }

module Ident = struct
  type t = ident
  let name id = Printing.mlvar_show id.name
  let name_full id = Printing.mlvar_show_full id.name
  let pp fmt id = Format.fprintf fmt "%s" (name id)
  let pp_mid fmt id = Format.fprintf fmt "%s(%s)" (name id)
      (Parsing.show_scope id.scope)
  let pp_full fmt id = Format.fprintf fmt "%s(%s)" (name_full id)
      (Parsing.show_scope id.scope)
  let external_name id =
    match MlVar.get_name id.name with
    | Some s -> s
    | None -> "%anon%"

  let of_identifier (env:Env.t) id : ident =
    let open Env in
    let open Parsing in
    let v, info = match IdentMap.find_opt id env.vars with
      | None -> Format.sprintf "id %s not found in %s %s!"
                  (PCI.to_string id)
                  (Parsing.show_block_kind env.current.kind)
                  env.current.name
                |> failwith
      | Some vi -> vi
    in
    { name = v
    ; scope = info.scope }

  let of_argument env arg : ident =
    of_identifier env arg.PC.Argument.identifier

  let compare i1 i2 =
    let c = MlVar.compare i1.name i2.name in
    if c <> 0 then c else compare i1.scope i2.scope
end

module IdentSet =
struct
  include Set.Make(Ident)
  let mem_ml mlv = exists (fun {name;_} -> MlVar.equal name mlv)
  let find_ml mlv = find_first (fun {name;_} -> MlVar.equal name mlv)
  let find_ml_opt mlv = find_first_opt (fun {name;_} -> MlVar.equal name mlv)
  let pp fmt s =
    let open Format in
    if is_empty s && !Utils.debug
    then fprintf fmt "ø"
    else Printing.pp_list ~sep:",@ " Ident.pp_mid fmt (to_list s)
end

type binop =
  | Add | Sub | Mult | Div | Mod | Pow | And | Or | Eq | Neq | Lt | Gt | Le | Ge
  | Is | Isn
type const =
  | None_
  (* | Ellipsis *)
  | Bool of bool
  | Int of int
  (* | Float of float *)
  | String of string
type scoped_identifiers = {
  nl_used : IdentSet.t;
  nl_unused : IdentSet.t;
  locals : IdentSet.t
}
type expr' =
  | Var of ident
  | Binop of expr * binop * expr
  | Cst of const
  | Lambda of spec * scoped_identifiers * expr
  | Apply of expr * params
  | Tuple of expr list
  (* | Projection of expr * expr (\* proj, value *\) *)
and expr = expr' annot
and spec =
  { posonly : (ident * expr option) list
  ; mixed   : (ident * expr option) list
  ; vararg  : ident option
  ; kwdonly : (ident * expr option) list
  ; kwdarg  : ident option }
and params =
  { pos : expr list
  ; kwd : (string * expr) list }

type target = ident

type instr' =
  | Block of instr list
  | FunDef of ident * spec * scoped_identifiers * instr
  | Return of expr option
  | Assign of target * expr
  | While of expr * instr
  | If of expr * instr * instr option
  | Iexpr of expr
  | Break | Continue
and instr = instr' annot

type prog = instr list * IdentSet.t

let dannot : 'a -> 'a annot = fun x -> Utils.dummy_pos, x
let env_annot env loc t = env.Env.to_loc loc, t

(** [scoped_identifier env bid] returns the pairs of sets of
    identifiers [s] where:
    - [s.used] is the set of global or non local identifiers that
      are accessed by [bid]
    - [s.unused] is the set of global or non local identifiers that
      are in scope but not used by [bid]
*)
let scoped_identifiers env bid =
  let open Parsing in
  let infos =  BidTable.find env.Env.infos bid in
  IdentMap.fold
    (fun ident (name, s) ({nl_used; nl_unused;locals} as acc) ->
       let id = { name; scope = s.scope } in
       match s.scope,infos.kind with
       | (Local _, _) | (Global, Module ) ->
         {acc with locals = IdentSet.(add id locals)}
       | _ ->
         if IdentMap.mem ident infos.identifiers then
           {acc with nl_used = IdentSet.(add id nl_used)}
         else
           {acc with nl_unused = IdentSet.(add id nl_unused)})
    env.vars IdentSet.{nl_used=empty;nl_unused=empty;locals=empty}

module Const = struct
  let of_constant (c:PC.Constant.t) : const = match c with
    | None -> None_
    | False -> Bool false
    | True -> Bool true
    | Integer i -> Int i
    (* | Float f -> Float f *)
    | String s -> String s
    (* | Ellipsis -> Ellipsis *)

    | Float _ | Ellipsis | BigInteger _ | Complex _ | ByteString _
      -> failwith "Not implemented (Const)."

  let to_gty c : MlGTy.t =
    let open Mlsem.Types in
    GTy.mk (match c with
        | None_ -> Sstt.Enum.mk "None" |> Sstt.Descr.mk_enum |> Sstt.Ty.mk_descr
        (* | Ellipsis -> failwith "Ty.Ellipsis" *)
        | Bool b -> if b then Ty.tt else Ty.ff
        | Int i -> Utils.ty_of_int i
        (* | Float _ -> Ty.float (\* !! TODO !! *\) *)
        | String _ -> Ty.string )
end

module Binop = struct
  let of_binop (op:PC.BinaryOperator.t) : binop = match op with
    | Add -> Add
    | Sub -> Sub
    | Mult -> Mult
    | Div -> Div
    | Mod -> Mod
    | Pow -> Pow

    | MatMult | LShift | RShift | BitOr | BitXor | BitAnd | FloorDiv
      -> failwith "Not implemented (Binop)."

  let of_boolop (op:PC.BooleanOperator.t) : binop = match op with
    | And -> And
    | Or -> Or

  let of_comparisonoperator (op:PC.ComparisonOperator.t) = match op with
    | Eq -> Eq
    | NotEq -> Neq
    | Lt -> Lt
    | Lte -> Le
    | Gt -> Gt
    | Gte -> Ge
    | Is -> Is
    | IsNot -> Isn

    | In | NotIn -> failwith "Not implemented (Binop)."

  let to_ml pos op =
    let open Utils in
    let int_op  = MT.(Arrow.mk Ty.int  (Arrow.mk Ty.int  Ty.int ))
    and int_cmp = MT.(Arrow.mk Ty.int  (Arrow.mk Ty.int  Ty.bool))
    and bool_op = MT.(Arrow.mk Ty.bool (Arrow.mk Ty.bool Ty.bool))
    and pol_cmp _ = let tv = mk_tv "bop_tv" in
      MT.(Arrow.mk tv    (Arrow.mk tv      Ty.bool)) in
    let strkey, ty = match op with
      | Add -> "+", int_op
      | Sub -> "-", int_op
      | Mult -> "*", int_op
      | Div -> "/", int_op
      | Mod -> "mod", int_op
      | Pow -> "^^", int_op
      | And -> "&&", bool_op
      | Or -> "||", bool_op
      | Eq -> "(=)", pol_cmp ()
      | Neq -> "<>", pol_cmp ()
      | Lt -> "<", int_cmp
      | Gt -> ">", int_cmp
      | Le -> "≤", int_cmp
      | Ge -> "≥", int_cmp
      | Is -> "is", pol_cmp ()
      | Isn -> "isnot", pol_cmp ()
    in
    let op_name = mk_id "%s" strkey in
    (match Builtins.find_opt op_name with
       Some v -> v
     | None -> Builtins.add op_name
                 (MlGTy.mk ty |> (mk_value dummy_pos)))
    |> var_of_vart pos
end

(*  ***  Pretty-printers  ***  *)

let pp_ident fmt id =
  Format.fprintf fmt "%a" Ident.pp id
let pp_binop fmt op =
  Format.fprintf fmt "%s"
    (match op with
     | Add  -> "+"
     | Sub  -> "-"
     | Mult -> "*"
     | Div  -> "/"
     | Mod  -> "%"
     | Pow  -> "^"
     | And  -> "&&"
     | Or   -> "||"
     | Eq   -> "="
     | Neq  -> "<>"
     | Lt   -> "<"
     | Gt   -> ">"
     | Le   -> "<="
     | Ge   -> ">="
     | Is   -> "is"
     | Isn  -> "isn't" )
let pp_const fmt = function
  | None_ -> Format.fprintf fmt "None"
  (* | Ellipsis -> Format.fprintf fmt "..." *)
  | Bool b -> Format.fprintf fmt "%s" (if b then "True" else "False")
  | Int i -> Format.fprintf fmt "%d" i
  (* | Float f -> Format.fprintf fmt "%.2f" f *)
  | String s -> Format.fprintf fmt "@[\"%s\"@]" s
let pp_identset fmt id =
  let open Format in
  (* let old_m = pp_get_margin fmt () in *)
  (* pp_set_margin fmt pp_infinity; *)
  fprintf fmt "@[%a@]" IdentSet.pp id
  (* ;pp_set_margin fmt old_m *)
let pp_scope fmt (si:scoped_identifiers) =
  if !Utils.debug then
    Format.fprintf fmt
      "@{<yellow>\"\"\"@\n@[<hov 2>n_l: %a (<- un…used ->) %a@]@\n\
       loc: @[%a@]@\n\"\"\"@}@\n"
      pp_identset si.nl_unused pp_identset si.nl_used pp_identset si.locals

let rec pp_expr' fmt =
  let open Format in
  function
  | Var id -> pp_ident fmt id
  | Binop (e1, b, e2) ->
    fprintf fmt "@[(%a %a %a)@]" pp_expr e1 pp_binop b pp_expr e2
  | Cst c -> pp_const fmt c
  | Lambda (x,si, e) ->
    fprintf fmt "@[%a@[<hov 2>@{<bold;green>lambda@} %a : %a@]@]"
      pp_scope si pp_spec x pp_expr e
  | Apply (e,p) -> fprintf fmt "@[%a%a@]" pp_expr e pp_params p
  | Tuple el ->
    fprintf fmt "@[<hov 1>(%a)@]" (Printing.pp_list ~sep:",@ " pp_expr) el
(*| Projection (p,e) -> Format.fprintf fmt "@[%a[%a]@]" pp_expr e pp_expr p *)
and pp_expr fmt (_,e') = pp_expr' fmt e'
and pp_spec fmt s =
  let open Format in
  let pr_if b str = if b then str else "" in
  let po, mx, va, ko, ka =
    s.posonly<>[], s.mixed<>[], s.vararg<>None, s.kwdonly<>[], s.kwdarg<>None in
  let pos_mix = (pr_if po ", /") ^ (pr_if (po && (mx||va||ko||ka)) ", ") in
  let var = match s.vararg with None -> "" | Some id -> Ident.name id in
  let mix_kwd = (pr_if (mx && (va || ko)) ", ") ^ (pr_if (va || ko) "*") ^ var
                ^ (pr_if ko ", ") in
  let kwo_kwa = pr_if (ka && (ko || va || mx)) ", " in
  let kwarg =
    match s.kwdarg with None -> "" | Some id -> "**" ^ Ident.(name id) in
  let pp_list_i_eo =
    Printing.pp_list ~sep:",@ " (fun fmt (i,(e:expr option)) ->
        fprintf fmt "%s%s%a" Ident.(name i) (if e = None then "" else "=")
          (pp_print_option pp_expr) e) in
  fprintf fmt "@[<hov 1>(%a%s@,%a%s@,%a%s@,%s)@]"
    pp_list_i_eo s.posonly
    pos_mix
    pp_list_i_eo s.mixed
    mix_kwd
    pp_list_i_eo s.kwdonly
    kwo_kwa
    kwarg
and pp_params fmt {pos;kwd} =
  let open Format in
  let open Printing in
  fprintf fmt "@[(%a%s%a)@]"
    (pp_list ~sep:",@ " pp_expr) pos
    (if pos<>[] && kwd<>[] then ", " else "")
    (pp_list ~sep:",@ " (fun fmt (k,e) ->
         fprintf fmt "@[%a=%a@]" pp_print_string k pp_expr e)) kwd

let rec pp_instr' fmt instr' : unit =
  let open Format in
  match instr' with
  | Block il ->
    if il = []
    then fprintf fmt "@[@{<bold>pass@} @{<green;italic># Empty block@}@]"
    else fprintf fmt
        (if !Utils.debug
         then "@[@{<green;italic># Block [@}@\n%a@\n\
               @{<green;italic># ] Block@}@]" else "%a")
        pp_instr_list il
  | Assign (x,e) -> fprintf fmt "@[<hov 2>@{<yellow;bold>%a@} = %a@]"
                      (pp_ident) x pp_expr e
  | FunDef (i,s,si,b) ->
    fprintf fmt
      "@[<hov 2>@{<green;bold>def@} @{<yellow;bold>%a@}%a@{<bold>:@}@\n%a%a@]"
      pp_ident i pp_spec s pp_scope si pp_instr b
  | While (e,i) ->
    fprintf fmt "@[<hov 2>@{<green;bold>while@} %a@{<bold>:@}@\n%a@]"
      pp_expr e pp_instr i
  | If (e,i,io) ->
    fprintf fmt
      "@[@{<green;bold>if@} %a@{<bold>:@}@\n  %a@\n\
       @{<green;bold>else:@}@\n  %a@]"
      pp_expr e pp_instr i
      (pp_print_option ~none:(fun fmt () -> Block [] |> pp_instr' fmt) pp_instr)
      io
  | Iexpr e -> pp_expr fmt e
  | Return eo ->
    fprintf fmt "@[<hov 2>@{<green;bold>return@}@ %a@]"
      (pp_print_option pp_expr) eo
  | Break -> fprintf fmt "@{<green;bold>break@}"
  | Continue -> fprintf fmt "@{<green;bold>continue@}"
and pp_instr fmt (_,instr') = pp_instr' fmt instr'
and pp_instr_list fmt il =
  Format.(fprintf fmt "%a"
            (Printing.pp_list ~sep:"@\n" pp_instr)
            il)

let pp_prog fmt (p,si:prog) =
  Format.fprintf fmt "%a%a@."
    (fun fmt set -> if !Utils.debug then
        Format.fprintf fmt "@{<yellow>\"\"\"@\n@[%a@]@\n\"\"\"@}@\n@\n"
          pp_identset set) si
    pp_instr_list p
