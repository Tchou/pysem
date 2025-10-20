type 'a annot = Mlsem.Common.Position.t * 'a

type ident =
  { name : string
  ; scope : Parsing.scope }
type binop =
  | Add | Sub | Mult | Div | Mod | Pow | And | Or | Eq | Neq | Lt | Gt | Le | Ge
type const =
  | None_
  | Bool of bool
  | Int of int
  | Float of float
  | String of string

type expr' =
  | Var of ident
  | Binop of expr * binop * expr
  | Cst of const
  | Apply of expr * params
and expr = expr' annot
and params =
  { pos : expr list
  ; kw : (string * expr) list }

type spec =
  { posonly : (ident * expr option) list
  ; args    : (ident * expr option) list
  ; vararg  : ident option
  ; kwonly  : (ident * expr option) list
  ; kwarg   : ident option }
type target = ident

type instr' =
  | Block of instr list
  | FunDef of ident * spec * instr list
  | Return of expr option
  | Assign of target * expr
  | While of expr * instr
  | If of expr * instr * instr option
  | Iexpr of expr
  | Break | Continue
and instr = instr' annot

type prog = instr list

let dummy_annot = Mlsem.Common.Position.dummy
let dannot : 'a -> 'a annot = fun x -> dummy_annot, x
let env_annot env loc t = env.Env.to_loc loc, t

module PC = PyreAst.Concrete

module Ident = struct
  let of_identifier env id : ident =
    let open Env in
    let open Parsing in
    let open PC in
    let info = match IdentMap.find_opt id env.current.identifiers with
      | None -> failwith (Printf.sprintf "id %s not found in %s %s!"
                            (Identifier.to_string id)
                            (Parsing.show_block_kind env.current.kind)
                            env.current.name)
      | Some i -> i
    in
    { name = Identifier.to_string id
    ; scope = info.scope }

  let of_argument env arg : ident =
    of_identifier env arg.PC.Argument.identifier
end

module Const = struct
  let of_constant _env (c:PC.Constant.t) : const = match c with
    | None -> None_
    | False -> Bool false
    | True -> Bool true
    | Integer i -> Int i
    | Float f -> Float f
    | String s -> String s

    | Ellipsis | BigInteger _ | Complex _ | ByteString _
      -> failwith "Not implemented (Const)."
end

module Binop = struct
  let of_binop _env (op:PC.BinaryOperator.t) : binop = match op with
    | Add -> Add
    | Sub -> Sub
    | Mult -> Mult
    | Div -> Div
    | Mod -> Mod
    | Pow -> Pow

    | MatMult | LShift | RShift | BitOr | BitXor | BitAnd | FloorDiv
      -> failwith "Not implemented (Binop)."

  let of_boolop _env (op:PC.BooleanOperator.t) : binop = match op with
    | And -> And
    | Or -> Or

  let of_comparisonoperator _env (op:PC.ComparisonOperator.t) = match op with
    | Eq -> Eq
    | NotEq -> Neq
    | Lt -> Lt
    | Lte -> Le
    | Gt -> Gt
    | Gte -> Ge

    | Is | IsNot | In | NotIn -> failwith "Not implemented (Binop)."
end

(*  ***  Pretty-printers  ***  *)

let pp_ident fmt id =
  Format.fprintf fmt "%s" (*Parsing.show_scope id.scope*) id.name
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
     | Ge   -> ">=" )
let pp_const fmt = function
  | None_ -> Format.fprintf fmt "None"
  | Bool b -> Format.fprintf fmt "%b" b
  | Int i -> Format.fprintf fmt "%d" i
  | Float f -> Format.fprintf fmt "%.2f" f
  | String s -> Format.fprintf fmt "@[\"%s\"@]" s

let pp_coma_list pp =
  Format.(pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp)

let rec pp_expr' fmt = function
  | Var id -> pp_ident fmt id
  | Binop (e1, b, e2) ->
     Format.fprintf fmt "@[(%a %a %a)@]" pp_expr e1 pp_binop b pp_expr e2
  | Cst c -> pp_const fmt c
  | Apply (e,p) -> Format.fprintf fmt "@[%a%a@]" pp_expr e pp_params p
and pp_expr fmt (_,e') = pp_expr' fmt e'
and pp_params fmt {pos;kw} =
  let open Format in
  fprintf fmt "@[(%a, %a)@]"
    (pp_coma_list pp_expr) pos
    (pp_coma_list (fun fmt (k,e) -> fprintf fmt "@[%s=%a@]" k pp_expr e)) kw

let pp_spec fmt s = (* should be simpler and correct*)
  let open Format in
  let pp_list_i_eo =
    pp_coma_list (fun fmt (i,(e:expr option)) ->
        fprintf fmt "%s%s%a" i.name (if e = None then "" else "=")
          (pp_print_option pp_expr) e) in
  let pos_arg = if s.posonly=[] && s.args=[] then ""
                else sprintf "%s/%s" (if s.posonly=[] then "" else ", ")
                       (if s.args=[] && s.kwonly=[] && s.vararg=None
                        then "" else ", ") in
  let arg_kw =
    if (s.posonly=[] && s.args=[] && s.vararg=None) then ""
    else sprintf "%s*%s%s"
           (if s.args=[] && s.posonly=[] then "" else ", ")
           (match s.vararg with None -> "" | Some i -> i.name)
           (if s.kwonly=[] then "" else ", ")
  in
  let kwo_kwa = if s.kwonly=[] || s.kwarg=None then "" else ", " in
  let kwarg = match s.kwarg with None -> "" | Some id -> "**" ^ id.name in
  fprintf fmt "@[(%a%s%a%s%a%s%s)@]"
    pp_list_i_eo s.posonly
    pos_arg
    pp_list_i_eo s.args
    arg_kw
    pp_list_i_eo s.kwonly
    kwo_kwa
    kwarg

let rec pp_instr' fmt instr' : unit =
  let open Format in
  let pp_instr_list il =
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") pp_instr il in
  match instr' with
  | Block il -> pp_instr_list fmt il
  | Assign (x,e) -> fprintf fmt "@[<hov 2>%a := %a@]"
                       (pp_ident) x pp_expr e
  | FunDef (i,s,il) -> fprintf fmt "@[<hov 2>def %a%a:@\n%a@]"
                         pp_ident i pp_spec s pp_instr_list il
  | While (e,i) -> fprintf fmt "@[<hov 2>while %a:@\n%a@]" pp_expr e pp_instr i
  | If (e,i,io) -> fprintf fmt "@[if %a:@\n  %a@\nelse:@\n  %a@]"
                     pp_expr e pp_instr i (pp_print_option pp_instr) io
  | Iexpr e -> pp_expr fmt e
  | Return eo -> pp_print_option pp_expr fmt eo
  | Break -> fprintf fmt "break@\n"
  | Continue -> fprintf fmt "continue@\n"
and pp_instr fmt (_,instr') = pp_instr' fmt instr'

let pp_prog prog =
  Format.(pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
            pp_instr prog)

module PAstPrinter = struct
  open Mlsem_app.PAst
  open Format

  let pp_const = Mlsem_lang.Const.pp
  let pp_projection = Mlsem_system.Ast.pp_projection

  let rec pp_pattern fmt p : unit =
    let pp_var_pattern fmt (v,p) =
      fprintf fmt "%s:@[%a@]" v pp_pattern p
    in
    let pp_list pp fmt l =
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ")
        pp fmt l in
    match p with
    | PatType _ -> fprintf fmt "PType"
    | PatVar (_,v) -> fprintf fmt "@[%s@]" v
    | PatLit c -> fprintf fmt "@[%a@]" pp_const c
    | PatTag _ -> fprintf fmt "PTag"
    | PatAnd (p1,p2) ->
       fprintf fmt "@[(%a) and (%a)@]" pp_pattern p1 pp_pattern p2
    | PatOr (p1,p2) ->
       fprintf fmt "@[(%a) or (%a)@]" pp_pattern p1 pp_pattern p2
    | PatTuple l -> pp_list pp_pattern fmt l
    | PatCons _ -> fprintf fmt "PCons"
    | PatRecord (l,b) ->
       (fprintf fmt "{@[%a@]%s}"
          (pp_list pp_var_pattern) l
          (if b then " .." else ""))
    | PatAssign ((_,v),c) -> fprintf fmt "P(%s:=%a)" v pp_const c
  and pp_ast (fmt:formatter) (ast:('a,'b,'c,'d,'e) Mlsem_app.PAst.ast) :unit =
    match ast with
    | Magic _ -> fprintf fmt "Magic"
    | Const c -> fprintf fmt "@[%a@]" pp_const c
    | Var v -> fprintf fmt "@[%s@]" v
    | Enum _ -> fprintf fmt "Enum"
    | Tag (_,t) -> pp_t fmt t
    | Suggest (v,_,t) -> fprintf fmt "@[<hov 2>Suggest %s:@ %a@]" v pp_t t
    | Lambda (v,_,t) -> fprintf fmt "@[<hov 2>fun %s ->@ %a@]" v pp_t t
    | LambdaRec l ->
       pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt "@\nand ")
         (fun fmt (v,_,t) -> fprintf fmt "@[<hov 2>rfun %s ->@ %a@]" v pp_t t)
         fmt
         l
    | Ite (test,_,t1,t2) ->
       fprintf fmt
         "@[@[<hov 2>if %a@]@\n@[<hov 2>then@ %a@]@\n@[<hov 2>else@ %a@]@]@ "
         pp_t test pp_t t1 pp_t t2
    | App (t1,t2) -> fprintf fmt "(@[%a@])@ (@[%a@])" pp_t t1 pp_t t2
    | Let ((_,v),t1,t2) ->
       fprintf fmt "@[@[<hov 2>let %s =@ @[%a@]@ in@]@\n%a@]" v pp_t t1 pp_t t2
    | Tuple l -> fprintf fmt "@[(%a)@]"
                   (pp_print_list
                      ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
                      (fun fmt t -> fprintf fmt "@[%a@]" pp_t t))
                   l
    | Cons (t1,t2) -> fprintf fmt "@[Cons(@[%a@],@[%a@])@]" pp_t t1 pp_t t2
    | Projection (p,t) -> fprintf fmt "@[<hov 2>proj(@[%a@],@ @[%a@])@]"
                            pp_projection p pp_t t
    | RecordUpdate (t,s,ot) ->
       let none = fun _ _ -> () in
       fprintf fmt "@[<hov 2>{upd %a@ %s@ %a}@]"
         pp_t t s (pp_print_option none) ot
    | TypeCast (t,_) -> fprintf fmt "@[<hov 2>cast [%a]@]" pp_t t
    | TypeCoerce (t,_,_) -> fprintf fmt "@[<hov 2>coerce [%a]@]" pp_t t
    | PatMatch (t,ptl) ->
       fprintf fmt "@[match @[%a@]@ with@\n| %a@]@\n"
         pp_t t (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "")
                   (fun fmt (p,t) -> fprintf fmt "| @[@[%a@] ->@ @[%a@]@]@\n"
                                       pp_pattern p pp_t t)) ptl
    | Cond (_t1,_,_t2,_ot) -> fprintf fmt "Cond"
    | While (test,_,body) ->
       fprintf fmt "@[<hov 2>while @[%a@]@ isn't false do@ %a@]"
         pp_t test pp_t body
    | Seq (t1,t2) -> fprintf fmt "@[<hov 2>Seq(%a,@ %a)@]" pp_t t1 pp_t t2
    | _ -> failwith "TODO"
  and pp_t fmt (_,ast) = pp_ast fmt ast
end
