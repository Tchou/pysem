open Aliases
(*
   See py-types.md
*)
(* Since we rely on the names we make always internal (not conditionally as in
   {Utils.export}, otherwise the code breaks if Utils.export is [true]) *)
let tag_s = Utils.internal "py_param_spec"
let tag = MT.Tag.define tag_s

let approx_id = MT.Enum.define (Utils.internal "py_param_approx_spec")

let exact_str = Utils.internal "py_param_exact_spec"
let field_name_pos i = Utils.mk_internal "p_%d" i
let field_name_kwd k = Utils.mk_internal "k_%s" k

let getter_name fmt =
  Format.kasprintf (fun s -> Utils.mk_internal "get_%s" s) fmt

let register_builtin name ast_builder arg =
  match Builtins.find_opt name with
  | Some v -> v
  | None ->
     let g = ast_builder arg in
     Builtins.add name g

let ast_get_field_or_default field =
  let open Utils in
  let tv = mk_tv field in
  let args = [ mk_rec_disj true [[ (field, (true, tv)) ]]
             ; tv ]
             |> MT.Tuple.mk in
  let fty = MT.Arrow.mk args tv |> MlGTy.mk in
  mk_value dummy_pos fty

let generic_getter p field_name arg odef =
  match odef with
  | None -> (* perform arg.field_name *)
     Utils.mk_projection p (MSAst.Field field_name) arg
  | Some def -> (* perform (ast_field_or_default field) arg def *)
     let open Utils in
     let g = register_builtin
               (getter_name "%s" field_name) ast_get_field_or_default field_name
             |> var_of_vart p
     in
     mk_app p g (mk_tuple p [ arg; def ])

let pos_getter p i arg odef =
  generic_getter p (field_name_pos i) arg odef

let kwd_getter p kwd arg odef =
  generic_getter p (field_name_kwd kwd) arg odef

let ast_get_field2 (field1, field2) =
  let open Utils in
  let tv = mk_tv field1 in
  let args =
    [ mk_rec_disj true
        [[ (field1, (false, tv)); (field2, (true, Sstt.Ty.empty)) ]]
    ; mk_rec_disj true
        [[ (field2, (false, tv)); (field1, (true, Sstt.Ty.empty)) ]] ]
    |> MT.Ty.disj
  in
  let fty = MT.Arrow.mk args tv |> MlGTy.mk in
  mk_value dummy_pos fty

let ast_get_field2_def (field1, field2) =
  let open Utils in
  let open Sstt in
  let tv = mk_tv field1 in
  let args =
    [ [ mk_rec_disj true
          [[ (field1, (false, tv)); (field2, (true, Ty.empty)) ]]; Ty.any ]
    ; [ mk_rec_disj true
          [[ (field2, (false, tv)); (field1, (true, Ty.empty)) ]]; Ty.any]
    ; [ mk_rec_disj true
          [[ (field2, (true, Ty.empty)); (field1, (true, Ty.empty)) ]]; tv] ]
    |> List.map MT.Tuple.mk
    |> Ty.disj
  in
  let fty = MT.Arrow.mk args tv |> MlGTy.mk in
  mk_value dummy_pos fty

let mix_getter p i kwd arg odef =
  let open Utils in
  let field1 = field_name_pos i in
  let field2 = field_name_kwd kwd in
  match odef with
  | None ->
     let g = register_builtin
               (getter_name "%s_%s" field1 field2)
               ast_get_field2 (field1, field2)
             |> var_of_vart p
     in
     mk_app p g arg
  | Some def ->
     let g = register_builtin
               (getter_name "%s_%s_def" field1 field2)
               ast_get_field2_def (field1, field2)
             |> var_of_vart p
     in
     mk_app p g (mk_tuple p [ arg; def ])

let approx_id_ty = MT.Enum.typ approx_id

type 'ty param =
  { pos : int
  ; name : string
  ; ty : 'ty
  ; has_default : bool }

type 'ty builder = 'ty param list * 'ty param list * 'ty param list

let empty = [], [], []

let validate (a,b,c) =
  a @ b @ c
  |> List.iteri (fun i p ->
         if p.pos <> i
         then fail "Missing argument for postision %d" i)
let rec insert p l =
  match l with
  | [] -> [p]
  | p'::ll ->
     if p'.pos < p.pos
     then p' :: (insert p ll)
     else if p'.pos = p.pos
     then fail "Multiple arguments for position %d (adding %s, existing %s)"
            p.pos p.name p'.name false
     else p :: l

let add_param (pb, ab, kb) (k : [`Pos|`Mix|`Kwd]) pos name ty has_default =
  let param = { pos; name; ty; has_default } in
  match k with
  | `Pos -> insert param pb, ab, kb
  | `Mix -> pb, insert param ab, kb
  | `Kwd -> pb, ab, insert param kb

let rec_as_pos l =
  List.map (fun p ->
      ( field_name_pos p.pos
      , (p.has_default, p.ty) )) l

let rec_as_kwd l =
  List.map (fun p ->
      ( field_name_kwd p.name
      , (p.has_default, p.ty) )) l

let map_split f l =
  let rec loop l acc =
    match l with
    | [] -> []
    | p :: ll ->
       let nacc = p::acc in
       (f (List.rev nacc) ll) :: loop ll nacc
  in
  (f [] l)::loop l []

let make_record_part (pb, mb, kb) =
  let always_pos = rec_as_pos pb in
  let always_kwd = rec_as_kwd kb in
  mb
  |> map_split (fun a_pos a_kwd ->
         always_pos
         @ rec_as_pos a_pos
         @ rec_as_kwd a_kwd
         @ always_kwd)

type 'ty param_kind =
  | Anon of 'ty param
  | Named of 'ty param
  | Str of string

let anon a = Anon { a with ty = () }
let named a = Named { a with ty = () }

module EnumHash = Hashtbl.Make(Sstt.Enum)
let descr_by_id = EnumHash.create 16

let id_by_descr = Hashtbl.create 16

let make_descr_part (pa, ma, ka) =
  let p = match List.map anon pa with
    | [] -> []
    | l -> l @ [ Str "/"]
  in
  let m = List.map named ma in
  let k = match List.map named ka with
    | [] -> []
    | l -> [Str "*"] @ l
  in
  let descr = p @ m @ k in
  let id = match Hashtbl.find_opt id_by_descr descr with
    | Some e -> e
    | None ->
       (* Avoid MLsem's Enum that performs hashconsing,
          we want a fresh internal id for the enum *)
       let e = Sstt.Enum.mk exact_str in
       EnumHash.add descr_by_id e descr;
       Hashtbl.add id_by_descr descr e;
       e
  in
  let ty_id = Sstt.(id |> Descr.mk_enum |> Ty.mk_descr) in
  MT.Ty.cup approx_id_ty ty_id

let build builder =
  validate builder;
  let tr = make_record_part builder |> Utils.mk_rec_disj false in
  let ts = make_descr_part builder in
  [tr; ts]
  |> MT.Tuple.mk
  |> MT.Tag.mk tag

let is_non_empty_rec r =
  MT.Ty.leq r MT.Record.any
  && not (MT.Ty.is_empty r)
  && (MT.TVOp.top_vars r |> MT.TVarSet.is_empty)

let extract_record t =
  let ty = MT.Tag.proj tag t in
  let r = MT.Tuple.proj 2 0 ty in
  if is_non_empty_rec r
  then r
  else fail "Invalid record component"

let pack p pos kwd =
  let pos_l, pos_e =
    List.(mapi (fun i e -> field_name_pos i, e) pos |> split) in
  let kwd_l, kwd_e =
    List.(map (fun (s, e) -> field_name_kwd s, e) kwd |> split) in
  let r = Utils.mk_record p (pos_l @ kwd_l) (pos_e @ kwd_e) in
  let e_approx_id = Utils.mk_enum p approx_id in
  let pair = Utils.mk_tuple p [r; e_approx_id] in
  Utils.mk_tag p tag pair

let unpack p e =
  Utils.(mk_proj_tag p tag e
         |> mk_proj_tuple p 2 0)

let map_exact f =
  List.map (function
      | Str _ as s -> s
      | Named p -> Named { p with ty = f p.ty }
      | Anon p -> Anon { p with ty = f p.ty })

let map_approx f l =
  List.map (fun (pl, kl) ->
      ( List.map f pl
      , List.map (fun (s, t) -> s, f t) kl )
    ) l

type 'ty descr =
  | Exact of 'ty param_kind list
  | Approx of bool * ('ty list * (string * 'ty) list) list

let map f d = match d with
  | Exact l -> Exact (map_exact f l)
  | Approx (b, l) -> Approx (b,map_approx f l)

let record_atom_to_sig node ctx r =
  let open Sstt in
  let open Records in
  let pos, kwd =
    r.Atom.bindings
    |> LabelMap.to_list
    |> List.fold_left (fun (apos, akwd) (l, (ty, opt)) ->
           assert (not opt);
           let ty = node ctx ty in
           (* Keep in sync with field_name_pos and
              field_name_kw at the top of the file *)
           match String.split_on_char '_' (Label.name l) with
           | ["%%p"; n] -> (int_of_string n, ty)::apos, akwd
           | ["%%k"; n] -> apos, (n, ty)::akwd
           | _ -> fail "Unexpected label %s" (Label.name l)
         ) ([], [])
  in
  List.( sort (fun (a,_) (b,_) -> Int.compare a b) pos |> map snd
       , sort (fun (a,_) (b,_) -> String.compare a b) kwd)

let to_approx node ctx rec_type =
  let open Sstt in
  let sigs =
    Op.Records.as_union (rec_type |> Ty.get_descr |> Descr.get_records) in
  let b = List.fold_left (fun acc a ->
              Ty.cup acc (Descr.mk_record a|>Ty.mk_descr))
            Ty.empty sigs
          |> Ty.equiv rec_type
  in
  Approx (b, List.map (record_atom_to_sig node ctx) sigs)

let field_param node ctx is_anon rec_type p =
  let field =
    if is_anon
    then field_name_pos p.pos
    else field_name_kwd p.name
  in
  MT.Record.proj rec_type field |> node ctx

let extract_id descr_id =
  let open Sstt in
  let id = Ty.diff descr_id approx_id_ty |> Ty.get_descr |> Descr.get_enums in
  match Enums.destruct id with
  | (true, [ id ]) -> Some id
  | _ -> None

let to_exact node ctx rec_type spec_id =
  let descr = EnumHash.find descr_by_id spec_id in
  let tr_kind = function
    | Str _ as s -> s
    | Anon p -> Anon { p with ty = field_param node ctx true rec_type p }
    | Named p -> Named { p with ty = field_param node ctx false rec_type p }
  in
  Exact (List.map tr_kind descr)

let tuple_get n t =
  let open Sstt in
  Tuples.get n (Descr.get_tuples (Ty.get_descr t))

let tuple_proj n i t =
  let open Sstt in
  tuple_get n t
  |> Op.TupleComp.proj i

let to_t node ctx comp : _ option =
  let open Sstt in
  let _, t = Op.TagComp.as_atom comp in
  let rec_type = MT.Tuple.proj 2 0 t in
  let descr = tuple_proj 2 1 t in
  match extract_id descr with
  | None -> Some (to_approx node ctx rec_type)
  | Some id -> Some (to_exact node ctx rec_type id)

let pp_param_pos fmt p =
  Format.fprintf fmt "%a%s"
    Sstt.Printer.print_descr p.ty
    (if p.has_default then " = ..." else "")

let pp_param fmt p =
  Format.fprintf fmt "%s:@ %a" p.name pp_param_pos p

let print_exact _prec _assoc fmt l =
  let open Format in
  let pr fmt e = match e with
    | Anon p -> pp_param_pos fmt p
    | Named p -> pp_param fmt p
    | Str s -> pp_print_string fmt s
  in
  fprintf fmt "@[<hov 1>(%a)@]"
    (Printing.pp_list ~sep:"," pr) l

let pp_approx_sig fmt (lpos, lkwd) =
  let open Format in
  let open Sstt in
  let l = List.map Either.left lpos in
  let l = l @ List.map Either.right lkwd in
  let (s, _, _) = Prec.varop_info Tuple in
  fprintf fmt "@[(";
  Prec.print_seq (fun fmt e ->
      match e with
      | Either.Left ty -> fprintf fmt "%a" Printer.print_descr ty
      | Either.Right (s, ty) -> fprintf fmt "%s=%a"s Printer.print_descr ty)
    s fmt l;
  fprintf fmt ")@]"

let print_approx prec assoc fmt b l =
  let open Format in
  let open Sstt in
  let (sym,_,_) as cup_info = Prec.(varop_info Cup) in
  let need_par = Prec.need_parentheses prec assoc cup_info in
  fprintf fmt "@[<hov 1>";
  if need_par then fprintf fmt "(";
  Prec.print_seq pp_approx_sig sym fmt l;
  if not b
  then (match l with [] -> fprintf fmt "..." | _ -> fprintf fmt ";@ ..." );
  if need_par then fprintf fmt ")";
  fprintf fmt "@]"

let print prec assoc fmt d =
  match d with
  | Exact e -> print_exact prec assoc fmt e
  | Approx (b, l) -> print_approx prec assoc fmt b l

let py_params_builder : Sstt.Printer.extension_builder =
  Sstt.Printer.builder ~to_t ~map ~print

let params =
  Sstt.Printer.{ aliases = []
               ; extensions = [ (MT.Tag.tag tag, py_params_builder) ] }

let () = MT.PEnv.add_printer_param params
(* The code below depends on printer param being initialized *)

let pp_mapping fmt m =
  match m with
  | [] -> ()
  | _ -> Format.fprintf fmt "@[<hov 1>[%a]@]"
           (Printing.pp_list ~sep:"," (fun fmt (_, s) -> MT.TVar.pp fmt s)) m

let pp_py_scheme fmt s =
  let vars, gty = MT.TyScheme.get s in
  let vars = MT.TVarSet.destruct vars in
  let mk v n = v, MT.TVar.(mk (kind v) (Some n)) in
  let new_names = match vars with
    | [] -> []
    | [_] -> ["X"]
    | [_; _] -> ["X"; "Y"]
    | [_; _; _] -> ["X"; "Y"; "Z"]
    | _ -> List.mapi (fun i _ -> ("X" ^ string_of_int i)) vars
  in
  let mapping = List.map2 mk vars new_names in
  let () = pp_mapping fmt mapping in
  let subst = mapping
              |> List.map (fun (x, n) -> x, MT.TVar.typ n)
              |> MT.Subst.construct
  in
  let pp_ty = Sstt.Printer.print_ty (MT.PEnv.printer_params ()) in
  let inf, sup = MT.GTy.destruct gty in
  let inf' = MT.Subst.apply subst inf in
  if MT.Ty.equiv inf sup
  then Format.fprintf fmt "@[%a@]" pp_ty inf'
  else
    let sup' = MT.Subst.apply subst sup in
    Format.fprintf fmt "@[@[%a@] <:@ Any <:@ @[%a@]@]"
      pp_ty inf'
      pp_ty sup'
