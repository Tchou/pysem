open Aliases

type 'a annot = MC.Position.t * 'a

type ident =
  { name : MlVar.t
  ; scope : Parsing.scope }

module Ident = struct
  type t = ident
  let show ({name;_}:ident) = Printing.mlvar_show name
  let pp fmt id = Format.fprintf fmt "%s" (show id)
  let pp_full fmt id = Format.fprintf fmt "%s(%s)" (MlVar.get_unique_name id.name)
      (Parsing.show_scope id.scope)
  let external_name ({name; _}: ident) =
    match MlVar.get_name name with
    | Some s -> s
    | None -> failwith "Ident.external_name: anonymous variable"

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
  let pp fmt s =
    Format.(pp_print_seq
              ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
              Ident.pp_full fmt (to_seq s))
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

type expr' =
  | Var of ident
  | Binop of expr * binop * expr
  | Cst of const
  | Lambda of spec * IdentSet.t * expr
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
  | FunDef of ident * spec * IdentSet.t * instr
  | Return of expr option
  | Assign of target * expr
  | While of expr * instr
  | If of expr * instr * instr option
  | Iexpr of expr
  | Break | Continue
and instr = instr' annot

type prog = instr list

let dannot : 'a -> 'a annot = fun x -> Utils.dummy_pos, x
let env_annot env loc t = env.Env.to_loc loc, t
let used_identifiers env bid =
  let open Parsing in
  let infos =  BidTable.find env.Env.infos bid in
  let idents =
    IdentMap.fold
      (fun ident _ acc ->
         let name, s = IdentMap.find ident env.vars in
         IdentSet.add ({name; scope=s.scope}) acc)
      infos.identifiers IdentSet.empty
  in
  idents

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
  if !Utils.debug
  then begin
    let old_m = pp_get_margin fmt () in
    pp_set_margin fmt pp_infinity;
    fprintf fmt "@[#idents: %a@]@\n" IdentSet.pp id;
    pp_set_margin fmt old_m
  end else pp_print_nothing fmt ()

let rec pp_expr' fmt =
  let open Format in
  function
  | Var id -> pp_ident fmt id
  | Binop (e1, b, e2) ->
    fprintf fmt "@[(%a %a %a)@]" pp_expr e1 pp_binop b pp_expr e2
  | Cst c -> pp_const fmt c
  | Lambda (x,idents, e) ->
    fprintf fmt "@[@[%a@]@[<hov 2>fun %a -> %a@]@]"
      pp_identset idents pp_spec x pp_expr e
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
  let var = match s.vararg with None -> "" | Some id -> Ident.show id in
  let mix_kwd = (pr_if (mx && (va || ko)) ", ") ^ (pr_if (va || ko) "*") ^ var
                ^ (pr_if ko ", ") in
  let kwo_kwa = pr_if (ka && (ko || va || mx)) ", " in
  let kwarg =
    match s.kwdarg with None -> "" | Some id -> "**" ^ Ident.(show id) in
  let pp_list_i_eo =
    Printing.pp_list ~sep:",@ " (fun fmt (i,(e:expr option)) ->
        fprintf fmt "%s%s%a" Ident.(show i) (if e = None then "" else "=")
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
     then fprintf fmt "@[pass # Empty block@]"
     else fprintf fmt
            (if !Utils.debug then "@[# Block [@\n%a# ] Block@]" else "%a")
            pp_instr_list il
  | Assign (x,e) -> fprintf fmt "@[<hov 2>%a = %a@]"
                      (pp_ident) x pp_expr e
  | FunDef (i,s,idents, b) ->
    fprintf fmt "@[@[%a@]@[<hov 2>def %a%a:@\n%a@]@]"
      pp_identset idents
      pp_ident i pp_spec s pp_instr b
  | While (e,i) -> fprintf fmt "@[<hov 2>while %a:@\n%a@]" pp_expr e pp_instr i
  | If (e,i,io) -> fprintf fmt "@[if %a:@\n  %a@\nelse:@\n  %a@]"
                     pp_expr e pp_instr i (pp_print_option pp_instr) io
  | Iexpr e -> pp_expr fmt e
  | Return eo -> fprintf fmt "@[<hov 2>return@ %a@]"
                   (pp_print_option pp_expr) eo
  | Break -> fprintf fmt "break"
  | Continue -> fprintf fmt "continue"
and pp_instr fmt (_,instr') = pp_instr' fmt instr'
and pp_instr_list fmt il =
<<<<<<< HEAD
  Format.(fprintf fmt "%a"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") pp_instr)
=======
  Format.(fprintf fmt "%a@\n"
            (Printing.pp_list ~sep:"@\n" pp_instr)
>>>>>>> 496ec4d (generic pp_list)
            il)

let pp_prog = pp_instr_list
