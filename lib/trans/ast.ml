type 'a annot = Mlsem.Common.Position.t * 'a

type ident = string
  (* { name : string *)
  (* ; scope : Parsing.scope } *)
type binop = Add | Sub | Mult | Div | Mod | Pow
type const =
  | Bool of bool
  | Int of int
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

type instr' =
  | Block of instr list
  | Assign of ident * expr
  | FunDef of ident * spec
  | While of expr * instr
  | If of expr * instr * instr option
  | Iexpr of expr
  | Return of expr option
and instr = instr' annot

type prog = instr list

let dummy_annot = Mlsem.Common.Position.dummy
let dannot : 'a -> 'a annot = fun x -> dummy_annot, x
let env_annot env loc t = env.Env.to_loc loc, t

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
       fprintf fmt "@[{@[<hov 2>upd %a@ %s@ %a@]}@]"
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
