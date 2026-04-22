open Aliases

let pp_begin =
  let f = ref true in
  fun fmt -> Format.fprintf fmt (if !f then (f:=false ; "") else "——@.")
and pp_end fmt = Format.fprintf fmt "@."

let pr str =
  let open Format in
  pp_begin std_formatter;
  printf "@{<bold>@{<cyan>%s@}:@}@." str;
  kfprintf pp_end std_formatter
and dbg_pr str =
  let open Format in
  if !Utils.debug
  then (
    pp_begin std_formatter;
    printf "@{<bold>@{<yellow>%s@}:@}@." str;
    kfprintf pp_end std_formatter
  ) else ikfprintf ignore std_formatter

let pp_list ?(sep:(unit,Format.formatter,unit) format=";@ ") =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt sep)
let pp_nel str = function [] -> "" | _ -> str

let mlvar_show = MlVar.(if !Utils.debug && not !Utils.export
                        then get_unique_name else show)
and mlvar_show_full = MlVar.get_unique_name

module MSAstPrinter = struct
  open MSAst
  open Format

  let pp_variable fmt v = Format.fprintf fmt "%s" (mlvar_show v)
  let pp_gty = MlGTy.pp
  let pp_ty = MT.Ty.pp
  let pp_tag = MT.Tag.pp
  let pp_projection fmt p =
    let open MSAst in
    match p with
    | PiField str -> pp_print_string fmt str
    | PiFieldOpt str -> fprintf fmt "?%s" str
    | Pi (n,i) -> begin match (n,i) with
        | 2,0 -> fprintf fmt "fst"
        | 2,1 -> fprintf fmt "snd"
        | _,i -> fprintf fmt "[%d]" i
      end
    | PiTag t -> fprintf fmt "@[#%a@]" pp_tag t
    | _ -> pp_projection fmt p
  let pp_constructor = MSAst.pp_constructor

  let pp_Constructor_arg fmt ((c:MSAst.constructor),tl) pp_t =
    match c with
    | Rec (sb_l,b) ->
      fprintf fmt "@[<hov 2>{ %a%s }@]"
        (pp_list (fun fmt (f,t) ->
             fprintf fmt "@[%s : %a@]"
               f pp_t t))
        (List.combine sb_l tl)
        (if b then "; .." else "")
    | Tuple i ->
      if List.length tl <> i then failwith "Wrong tuple constructor!"
      else
        let col = Colors.next_color () in
        fprintf fmt "@[<hov 2>@{<bold;%s>(@} %a @{<bold;%s>)@}@]"
          col (pp_print_list
                 ~pp_sep:(fun fmt () -> fprintf fmt "@{<bold;%s>,@}@ " col)
                 pp_t)
          tl col
    | Tag t -> fprintf fmt "@[%a#[%a]@]" pp_tag t (pp_list ~sep:",@ " pp_t) tl
    | _ ->
      let col = Colors.next_color () in
      fprintf fmt "@[<hov 2>%a@{<bold;%s>(@}%a@{<bold;%s>)@}@]"
        pp_constructor c col (pp_list pp_t) tl col

  let pp_projection_arg fmt (p,t) pp_t =
    let open MSAst in
    match p with
    | PiField str -> fprintf fmt "@[@[%a@].%s@]" pp_t t str
    | PiFieldOpt str -> fprintf fmt "@[@[%a@]?.%s@]" pp_t t str
    | Pi (n,i) -> begin match (n,i) with
        | 2,0 -> fprintf fmt "fst @[%a@]" pp_t t
        | 2,1 -> fprintf fmt "snd @[%a@]" pp_t t
        | _,i -> fprintf fmt "@[%a@]@,.[%d]" pp_t t i
      end
    | PiTag tag -> fprintf fmt "@[@[%a@]@,#%a@]" pp_t t pp_tag tag
    | _ -> fprintf fmt "@[@[%a@].@[%a@]@]" pp_t t pp_projection p

  let rec pp_operation_recupd fmt t f0 =
    let open Format in
    let rec get_recupd acc ti =
      match ti with
      | _, Operation (RecUpd fn, tn) -> begin match tn with
          | _, Constructor (Tuple 2, [tm; vn])->
            get_recupd ((fn, Some vn)::acc) tm
          | _ -> tn, (fn,None)::acc
        end
      | _ -> ti, acc
    in
    match t with
    | _, Constructor (Tuple 2, [t1; t2]) ->
      let r, xvl = get_recupd [f0, Some t2] t1 in
      fprintf fmt "@[<hov 2>{ %a with@ %a }@]"
        pp_t r
        (pp_list (fun fmt (field, oval) ->
             fprintf fmt "%s%a" field
               (pp_print_option (fun fmt v ->
                    fprintf fmt " =@ %a" pp_t v)) oval)) xvl
    | _ -> fprintf fmt "@[<hov 2>{ %a with@ %s }@]" pp_t t f0
  and pp_operation_recdel fmt t field =
    let rec get_recdel acc ti = match ti with
      | _, Operation (RecDel fn, tn) -> get_recdel (fn::acc) tn
      | _ -> ti, acc
    in
    let r, x_l = get_recdel [field] t in
    fprintf fmt "@[<hov 2>{ %a without %a }@]" pp_t r
      (pp_list pp_print_string) x_l

  and pp_e (fmt:formatter) (e:MSAst.e) :unit =
    match e with
    | Value gty -> fprintf fmt "@[<hov 2>%a@]" pp_gty gty
    | Var v -> fprintf fmt "@[%a@]" pp_variable v
    | Constructor (c,tl) -> pp_Constructor_arg fmt (c,tl) pp_t
    | Lambda (gty,v,t) ->
      fprintf fmt "@[<hov 2>fun %a@ @{<bold;purple>: @[%a@]@} ->@ %a@]"
        pp_variable v pp_gty gty pp_t t
    | LambdaRec l ->
      pp_list
        ~sep:"@\nand "
        (fun fmt (gty,v,t) ->
           fprintf fmt "@[<hov 2>rfun %a@ @{<bold;purple>: @[%a@]@} ->@ %a@]"
            pp_variable v pp_gty gty pp_t t)
        fmt
        l
    | Ite (test,ty,t1,t2) ->
      fprintf fmt
        "@[<v>@[<hov 2>if %a is %a@]@ @[<hov 2>then@ %a@]@ @[else %a@]@]"
        pp_t test pp_gty ty pp_t t1 pp_t t2
    | App (t1,t2) ->
      let col = Colors.next_color () in
      fprintf fmt "@[<hov 2>@{<bold;%s>(@}@[%a@]@ @[%a@]@{<bold;%s>)@}@]"
        col pp_t t1 pp_t t2 col
    | Operation (op, t) -> begin match op with
        | RecUpd field -> pp_operation_recupd fmt t field
        | RecDel field -> pp_operation_recdel fmt t field
        | OCustom _ ->
          fprintf fmt "@[<hov 2>@[%a@].@[%a@]@]"
            pp_t t pp_operation op
      end
    | Projection (p,t) -> pp_projection_arg fmt (p,t) pp_t
    | Let (tyl,v,t1,t2) ->
      fprintf fmt "@[@[<hov 2>let %a@{<bold;purple>%s@[%a@]@} =@ @[%a@]@ in@]@\n%a@]"
        pp_variable v (pp_nel " : " tyl) (pp_list pp_ty) tyl pp_t t1 pp_t t2
    | TypeCast (t,ty,_) ->
      fprintf fmt "@[<hov 2>@{<bold;purple>(@}%a@{<bold;purple>)@ :> @[%a@]@}@]"
        pp_t t pp_gty ty
    | TypeCoerce (t,gty,_) ->
      fprintf fmt "@[<hov 2>@{<bold;purple>(@}%a@{<bold;purple>)@ <: @[%a@]@}@]"
        pp_t t pp_gty gty
    | Alt (t1,t2) -> fprintf fmt "@[<hov 2>Alt(%a,@ %a)@]" pp_t t1 pp_t t2
  and pp_t fmt (_,e) = pp_e fmt e
end

module MLAstPrinter = struct
  open MLAst
  open Format

  let pp_variable = MSAstPrinter.pp_variable
  let pp_gty = MlGTy.pp
  let pp_ty = MT.Ty.pp
  let pp_projection = MSAstPrinter.pp_projection
  let pp_operation = SA.pp_operation

  let rec pp_operation_recupd fmt t f0 =
    let open Format in
    let rec get_recupd acc ti = match ti with
      | _, Operation (RecUpd fn, tn) -> begin match tn with
          | _, Constructor (Tuple 2, [tm; vn])->
            get_recupd ((fn, Some vn)::acc) tm
          | _ -> tn, (fn,None)::acc
        end
      | _ -> ti, acc
    in
    match t with
    | _, Constructor (Tuple 2, [t1; t2]) ->
      let r, xvl = get_recupd [f0, Some t2] t1 in
      fprintf fmt "@[<hov 2>{ %a with@ %a }@]"
        pp_t r
        (pp_list (fun fmt (field, oval) ->
             fprintf fmt "%s%a" field
               (pp_print_option (fun fmt v ->
                    fprintf fmt " =@ %a" pp_t v)) oval)) xvl
    | _ -> fprintf fmt "@[<hov 2>{ %a with@ %s }@]" pp_t t f0
  and pp_operation_recdel fmt t field =
    let rec get_recdel acc ti = match ti with
      | _, Operation (RecDel fn, tn) -> get_recdel (fn::acc) tn
      | _ -> ti, acc
    in
    let r, x_l = get_recdel [field] t in
    fprintf fmt "@[<hov 2>{ %a without %a }@]" pp_t r
      (pp_list pp_print_string) x_l

  and pp_pattern_constructor fmt pc : unit = match pc with
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
    | Constructor (c,tl) -> MSAstPrinter.pp_Constructor_arg fmt (c,tl) pp_t
    | Lambda (_,gty,v,t) ->
      fprintf fmt "@[<hov 2>fun %a@ @{<bold;purple>: @[%a@]@} ->@ %a@]"
        pp_variable v pp_gty gty pp_t t
    | LambdaRec l ->
      pp_list
        ~sep:"@\nand "
        (fun fmt (gty,v,t) -> fprintf fmt "@[<hov 2>rfun %a@ @{<bold;purple>: @[%a@]@} ->@ %a@]"
            pp_variable v pp_gty gty pp_t t)
        fmt
        l
    | Ite (test,ty,t1,t2) ->
      fprintf fmt
        "@[<v>@[<hov 2>if %a is %a@]@ @[<hov 2>then@ %a@]@ @[else %a@]@]"
        pp_t test pp_gty ty pp_t t1 pp_t t2
    | PatMatch (t,ptl) ->
      fprintf fmt "@[match @[%a@]@ with@ | %a@]"
        pp_t t (pp_list ~sep:""
                  (fun fmt (p,t) -> fprintf fmt "| @[@[%a@] ->@ @[%a@]@]"
                      pp_pattern p pp_t t)) ptl
    | App (t1,t2) ->
      let col = Colors.next_color () in
      fprintf fmt "@[<hov 2>@{<bold;%s>(@}@[%a@]@ @[%a@]@{<bold;%s>)@}@]@]"
        col pp_t t1 pp_t t2 col
    | Operation (op, t) -> begin match op with
        | RecUpd field -> pp_operation_recupd fmt t field
        | RecDel field -> pp_operation_recdel fmt t field
        | OCustom _ ->
          fprintf fmt "@[<hov 2>@[%a@].@[%a@]@]"
            pp_t t pp_operation op
      end
    | Projection (p,t) -> MSAstPrinter.pp_projection_arg fmt (p,t) pp_t
    | Declare (v,t) ->
      fprintf fmt "@[<hov 2>val mut %a =@ %a@]"
        pp_variable v pp_t t
    | Let (tyl,v,t1,t2) ->
      fprintf fmt "@[@[<hov 2>let %a@{<bold;purple>%s@[%a@]@} =@ @[%a@]@ in@]@\n%a@]"
        pp_variable v (pp_nel " : " tyl) (pp_list pp_ty) tyl pp_t t1 pp_t t2
    | TypeCast (t,ty,_) ->
      fprintf fmt "@[<hov 2>@{<bold;purple>(@}%a@{<bold;purple>)@ :> @[%a@]@}@]"
        pp_t t pp_gty ty
    | TypeCoerce (t,gty,_) ->
      fprintf fmt "@[<hov 2>@{<bold;purple>(@}%a@{<bold;purple>)@ <: @[%a@]@}@]"
        pp_t t pp_gty gty
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
        pp_t test pp_gty ty pp_t t (pp_print_option pp_t) ot
    | While (test,ty,body) ->
      fprintf fmt "@[<hov 2>while @[%a@]@ isn't @[%a@] do@ %a@]"
        pp_t test pp_gty ty pp_t body
    | Return t -> fprintf fmt "@[<hov 2>return %a@]" pp_t t
    | Break -> fprintf fmt "break"

  and pp_t fmt (_,e) = pp_e fmt e
end
module PAstPrinter = struct
  open Mlsem_app.PAst
  open Format

  let pp_const = ML.Const.pp
  let pp_projection = MSAstPrinter.pp_projection

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
      pp_list
        ~sep:"@\nand "
        (fun fmt (v,_,t) -> fprintf fmt "@[<hov 2>rfun %s ->@ %a@]" v pp_t t)
        fmt
        l
    | Ite (test,_,t1,t2) ->
      fprintf fmt
        "@[<v>@[<hov 2>if %a@]@ @[<hov 2>then@ %a@]@ @[else %a@]@]"
        pp_t test pp_t t1 pp_t t2
    | App (t1,t2) -> fprintf fmt "(@[%a@])@ (@[%a@])" pp_t t1 pp_t t2
    | Let ((_,v),t1,t2) ->
      fprintf fmt "@[@[<hov 2>let %s =@ @[%a@]@ in@]@\n%a@]" v pp_t t1 pp_t t2
    | Tuple l -> fprintf fmt "@[(%a)@]"
                   (pp_list ~sep:",@ " pp_t)
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
      fprintf fmt "@[match @[%a@]@ with@\n| %a@]"
        pp_t t (pp_list ~sep:""
                  (fun fmt (p,t) -> fprintf fmt "| @[@[%a@] ->@ @[%a@]@]"
                      pp_pattern p pp_t t)) ptl
    | Cond (_t1,_,_t2,_ot) -> fprintf fmt "Cond"
    | While (test,_,body) ->
      fprintf fmt "@[<hov 2>while @[%a@]@ isn't false do@ %a@]"
        pp_t test pp_t body
    | Seq (t1,t2) -> fprintf fmt "@[<hov 2>Seq(%a,@ %a)@]" pp_t t1 pp_t t2
    | _ -> failwith "Not implemented (PAstPrinter)."
  and pp_t fmt (_,ast) = pp_ast fmt ast
end

let pp_ml_top fmt (v,ml) =
  let open MLAstPrinter in
  Format.fprintf fmt "@[<hov 2>let %a =@ %a@]" pp_variable v pp_t ml

let pp_ml_tys fmt (v,tys) =
  let open MLAstPrinter in
  Format.fprintf fmt "@[<hov 2>val %a :@ %a@]"
    pp_variable v MT.TyScheme.pp tys
