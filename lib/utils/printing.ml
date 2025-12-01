open Aliases

let pp_sep fmt = Format.fprintf fmt "@.——@\n"

let pr str =
  let open Format in
  printf "@{<bold>@{<cyan>%s@}:@}@." str;
  kfprintf pp_sep std_formatter
and dbg_pr str =
  let open Format in
  if !Utils.debug
  then (
    printf "@{<bold>@{<yellow>%s@}:@}@." str;
    kfprintf pp_sep std_formatter
  ) else ikfprintf ignore std_formatter

let pp_list ?(sep=";") pp fmt l =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "%s@ "sep)
    pp fmt l
let pp_nel str = function [] -> "" | _ -> str

let mlvar_show = MlVar.(if !Utils.debug && not !Utils.export
                        then get_unique_name else show)

module PAstPrinter = struct
  open Mlsem_app.PAst
  open Format

  let pp_const = ML.Const.pp
  let pp_projection fmt p =
    let open MSAst in
    match p with
    | Field str -> pp_print_string fmt str
    | FieldOpt str -> fprintf fmt "?%s" str
    | Pi (_,i) -> fprintf fmt "[%d]" i
    | _ -> pp_projection fmt p

  let rec pp_pattern fmt p : unit =
    let pp_var_pattern fmt (v,p) = fprintf fmt "%s:@[%a@]" v pp_pattern p in
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
         "@[<hov 2>if %a@]@\n@[<hov 2>then@ %a@]@\n"
         pp_t test pp_t t1;
       fprintf fmt (match t2 with (_, Ite _) -> "@[else %a@]@ "
                                | _ -> "@[<hov 2>else@ %a@]@ ")
         pp_t t2
    | App (t1,t2) -> fprintf fmt "(@[%a@])@ (@[%a@])" pp_t t1 pp_t t2
    | Let ((_,v),t1,t2) ->
       fprintf fmt "@[@[<hov 2>let %s =@ @[%a@]@ in@]@\n%a@]" v pp_t t1 pp_t t2
    | Tuple l -> fprintf fmt "@[(%a)@]"
                   (pp_list ~sep:"," pp_t)
                   l
    | Cons (t1,t2) -> fprintf fmt "@[Cons(@[%a@],@[%a@])@]" pp_t t1 pp_t t2
    | Projection (p,t) -> fprintf fmt "@[<hov 2>proj(@[%a@],@ @[%a@])@]"
                            pp_projection p pp_t t
    | RecordUpdate (t,s,ot) ->
       let none = fun _ _ -> () in
       fprintf fmt "@[<hov 2>{upd %a@ %s@ %a}@]"
         pp_t t s (pp_print_option none) ot
    | TypeCast (t,_,_) -> fprintf fmt "@[<hov 2>cast [%a]@]" pp_t t
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
    | _ -> failwith "Not implemented (PAstPrinter)."
  and pp_t fmt (_,ast) = pp_ast fmt ast
end

module MLAstPrinter = struct
  open MLAst
  open Format

  let pp_variable fmt v = Format.fprintf fmt "%s" (mlvar_show v)
  let pp_gty = MlGTy.pp
  let pp_ty = MT.Ty.pp
  let pp_const = ML.Const.pp
  let pp_projection = PAstPrinter.pp_projection
  let pp_constructor = Mlsem.System.Ast.pp_constructor

  let pp_Constructor_arg fmt ((c:MSAst.constructor),tl) pp_t = match c with
    | Rec (sb_l,b) ->
       fprintf fmt "@[<hov 2>{ %a%s }@]"
         (pp_list (fun fmt ((f,opt),t) ->
              fprintf fmt "@[%s :%s %a@]"
                f (if opt then "?" else "") pp_t t))
         (List.combine sb_l tl)
         (if b then "; .." else "")
    | Tuple i ->
       if List.length tl <> i then failwith "Wrong tuple constructor!"
       else fprintf fmt "@[<hov 2>(%a)@]" (pp_list ~sep:"," pp_t) tl
    | _ -> fprintf fmt "@[<hov 2>%a(%a)@]" pp_constructor c (pp_list pp_t) tl

  let rec pp_pattern_constructor fmt pc : unit = match pc with
    | PCTuple i -> fprintf fmt "PTuple(%d)" i
    | PCCons -> fprintf fmt "PCCons"
    | PCRec (sl,b) ->
       fprintf fmt "@[PCR(%a,%b)@]" (pp_list pp_print_string) sl b
    | PCTag _ -> fprintf fmt "PCTag"
    | PCEnum _ -> fprintf fmt "PCEnum"
    | PCCustom _ -> fprintf fmt "PCCustom"
  and pp_pattern fmt p : unit = match p with
    | PType _ -> fprintf fmt "PType"
    | PVar (_,v) -> fprintf fmt "@[%a@]" pp_variable v
    | PConstructor (pc,pl) ->
       fprintf fmt "@[<hov 2>PCtor(%a;@ %a)@]"
         pp_pattern_constructor pc (pp_list pp_pattern) pl
    | PAnd (p1,p2) ->
       fprintf fmt "@[(%a) and (%a)@]" pp_pattern p1 pp_pattern p2
    | POr (p1,p2) ->
       fprintf fmt "@[(%a) or (%a)@]" pp_pattern p1 pp_pattern p2
    | PAssign (_,v,gty) -> fprintf fmt "P(%a:=%a)" pp_variable v pp_gty gty
  and pp_e (fmt:formatter) (e:MLAst.e) :unit =
    match e with
    | Hole i -> fprintf fmt "Hole(%d)" i
    | Exc -> fprintf fmt "Exc"
    | Void -> fprintf fmt "Void"
    | Voidify t -> fprintf fmt "@[<hov 2>Vdfy %a@]" pp_t t
    | Isolate t -> fprintf fmt "@[<hov 2>Islt %a@]" pp_t t
    | Value gty -> fprintf fmt "@[<hov 2>%a@]" pp_gty gty
    | Var v -> fprintf fmt "@[%a@]" pp_variable v
    | Constructor (c,tl) -> pp_Constructor_arg fmt (c,tl) pp_t
    | Lambda (_,gty,v,t) ->
       fprintf fmt "@[<hov 2>fun %a@ : @[%a@] ->@ %a@]"
         pp_variable v pp_gty gty pp_t t
    | LambdaRec l ->
       pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt "@\nand ")
         (fun fmt (gty,v,t) -> fprintf fmt "@[<hov 2>rfun %a@ : @[%a@] ->@ %a@]"
                                 pp_variable v pp_gty gty pp_t t)
         fmt
         l
    | Ite (test,ty,t1,t2) ->
       fprintf fmt
         "@[<hov 2>if %a is %a@]@\n@[<hov 2>then@ %a@]@\n"
         pp_t test pp_ty ty pp_t t1;
       fprintf fmt (match t2 with (_, Ite _) -> "@[else %a@]@ "
                                | _ -> "@[<hov 2>else@ %a@]@ ")
         pp_t t2
    | PatMatch (t,ptl) ->
       fprintf fmt "@[match @[%a@]@ with@ | %a@]@\n"
         pp_t t (pp_print_list ~pp_sep:pp_print_nothing
                   (fun fmt (p,t) -> fprintf fmt "| @[@[%a@] ->@ @[%a@]@]@\n"
                                       pp_pattern p pp_t t)) ptl
    | App (t1,t2) -> fprintf fmt "@[<hov 2>(@[%a@]@ @[%a@])@]" pp_t t1 pp_t t2
    | Projection (p,t) -> fprintf fmt "@[<hov 2>@[%a@].@[%a@]@]"
                            pp_t t pp_projection p
    | Declare (v,t) ->
       fprintf fmt "@[<hov 2>val mut %a =@ %a@]@\n"
         pp_variable v pp_t t
    | Let (tyl,v,t1,t2) ->
       fprintf fmt "@[@[<hov 2>let %a%s@[%a@] =@ @[%a@]@ in@]@\n%a@]"
         pp_variable v (pp_nel " : " tyl) (pp_list pp_ty) tyl pp_t t1 pp_t t2
    | TypeCast (t,ty,_) ->
       fprintf fmt "@[<hov 2>cast [%a]@ to @[%a@]@]" pp_t t pp_ty ty
    | TypeCoerce (t,gty,_) ->
       fprintf fmt "@[<hov 2>coerce [%a]@ to @[%a@]@]" pp_t t pp_gty gty
    | VarAssign (v,t) -> fprintf fmt "@[<hov 2>%a :=@ %a@]"
                           pp_variable v pp_t t
    | Loop t -> fprintf fmt "@[<hov 2>loop:@ %a@]" pp_t t
    | Try (t1,t2) -> fprintf fmt "@[<hov 2>try@ %a@]@ @[<hov 2>with@ %a@]"
                       pp_t t1 pp_t t2
    | Seq (t1,t2) -> fprintf fmt "@[<hov 2>%a;@ %a@]" pp_t t1 pp_t t2
    | Alt (t1,t2) -> fprintf fmt "@[<hov 2>Alt(%a,@ %a)@]" pp_t t1 pp_t t2
    | Block (_,t) -> fprintf fmt "@[<hov 2>Block:@ %a@]" pp_t t
    | Ret (_,ot) -> fprintf fmt "@[<hov 2>Ret:@ %a@]"
                     (pp_print_option pp_t) ot
    | If (test,ty,t,ot) ->
       fprintf fmt
         "@[@[<hov 2>if: %a is %a@]@\n@[<hov 2>then@ %a@]@\n\
          @[<hov 2>else:@ %a@]@]@ "
         pp_t test pp_ty ty pp_t t (pp_print_option pp_t) ot
    | While (test,ty,body) ->
       fprintf fmt "@[<hov 2>while @[%a@]@ isn't @[%a@] do@ %a@]"
         pp_t test pp_ty ty pp_t body
    | Return t -> fprintf fmt "@[<hov 2>return %a@]" pp_t t
    | Break -> fprintf fmt "break"
  and pp_t fmt (_,e) = pp_e fmt e
end

module MSAstPrinter = struct
  open MSAst
  open Format

  let pp_variable = MLAstPrinter.pp_variable
  let pp_gty = MlGTy.pp
  let pp_ty = MT.Ty.pp
  let pp_const = ML.Const.pp
  let pp_projection = MLAstPrinter.pp_projection
  let pp_constructor = MSAst.pp_constructor

  let rec pp_e (fmt:formatter) (e:MSAst.e) :unit =
    match e with
    | Value gty -> fprintf fmt "@[<hov 2>(%a)@]" pp_gty gty
    | Var v -> fprintf fmt "@[%a@]" pp_variable v
    | Constructor (c,tl) -> MLAstPrinter.pp_Constructor_arg fmt (c,tl) pp_t
    | Lambda (gty,v,t) ->
       fprintf fmt "@[<hov 2>fun %a@ : @[%a@] ->@ %a@]"
         pp_variable v pp_gty gty pp_t t
    | LambdaRec l ->
       pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt "@\nand ")
         (fun fmt (gty,v,t) -> fprintf fmt "@[<hov 2>rfun %a@ : @[%a@] ->@ %a@]"
                                 pp_variable v pp_gty gty pp_t t)
         fmt
         l
    | Ite (test,ty,t1,t2) ->
       fprintf fmt
         "@[<hov 2>if %a is %a@]@\n@[<hov 2>then@ %a@]@\n"
         pp_t test pp_ty ty pp_t t1;
       fprintf fmt (match t2 with (_, Ite _) -> "@[else %a@]@ "
                                | _ -> "@[<hov 2>else@ %a@]@ ")
         pp_t t2
    | App (t1,t2) -> fprintf fmt "@[<hov 2>(@[%a@]@ @[%a@])@]" pp_t t1 pp_t t2
    | Projection (p,t) -> fprintf fmt "@[<hov 2>@[%a@].@[%a@]@]"
                            pp_t t pp_projection p
    | Let (tyl,v,t1,t2) ->
       fprintf fmt "@[@[<hov 2>let %a%s@[%a@] =@ @[%a@]@ in@]@\n%a@]"
         pp_variable v (pp_nel " : " tyl) (pp_list pp_ty) tyl pp_t t1 pp_t t2
    | TypeCast (t,ty,_) ->
       fprintf fmt "@[<hov 2>cast [%a]@ to @[%a@]@]" pp_t t pp_ty ty
    | TypeCoerce (t,gty,_) ->
       fprintf fmt "@[<hov 2>coerce [%a]@ to @[%a@]@]" pp_t t pp_gty gty
    | Alt (t1,t2) -> fprintf fmt "@[<hov 2>Alt(%a,@ %a)@]" pp_t t1 pp_t t2
  and pp_t fmt (_,e) = pp_e fmt e
end

let pp_ml_top fmt (v,ml) =
  let open MLAstPrinter in
  Format.fprintf fmt "@[<hov 2>let %a =@ %a@]@\n" pp_variable v pp_t ml

let pp_ml_tys fmt (v,tys) =
  let open MLAstPrinter in
  Format.fprintf fmt "@[<hov 2>val %a :@ %a@]@\n"
    pp_variable v MT.TyScheme.pp tys
