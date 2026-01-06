open Aliases
let tag_s = "py_param_spec"


let tag = MT.Tag.define tag_s
let dummy = MT.Enum.define "%%dummy_py_param_spec%%"
let dummy_t = dummy |> MT.Enum.typ

let fail msg = Format.kfprintf (fun _ -> assert false) Format.err_formatter msg

type 'ty param =
  { pos : int;
    name : string;
    ty : 'ty;
    has_default : bool }

type 'ty t = 'ty param list * 'ty param list * 'ty param list

let empty = [], [], []
let rec insert p l =
  match l with
    [] -> [p]
  | p'::ll ->
    if p'.pos < p.pos then p' :: (insert p ll)
    else if p'.pos = p.pos then
      fail "Multiple arguments for position %d (adding %s, existing %s)"
        p.pos p.name p'.name false
    else p :: l

let add_param (pb, ab, kb) (k : [`Pos|`Arg|`Kwd]) pos name ty has_default =
  let param = { pos; name; ty; has_default } in
  match k with
    `Pos -> insert param pb, ab, kb
  | `Arg -> pb, insert param ab, kb
  | `Kwd -> pb, ab, insert param kb


let rec_as_pos l =
  List.map (fun p ->
      Utils.field_name_pos p.pos,(p.has_default, p.ty)) l

let rec_as_kw l =
  List.map (fun p ->
      Utils.field_name_kw p.name,(p.has_default, p.ty)) l

let map_split f l =
  let rec loop l acc =
    match l with
      [] -> [ ]
    | p :: ll ->
      let nacc = p::acc in
      (f (List.rev nacc) ll) :: loop ll nacc
  in
  (f [] l)::loop l []

let make_record_part (pb, ab, kb) =
  let always_pos = rec_as_pos pb in
  let always_kw = rec_as_kw kb in
  ab
  |> map_split (fun a_pos a_kw ->
      always_pos
      @ rec_as_pos a_pos
      @ rec_as_kw a_kw
      @ always_kw)
let tt = MT.Enum.(define "true" |> typ)
let ff = MT.Enum.(define "false" |> typ)
let param_to_type p =
  let name_t = MT.Enum.(define p.name|>typ) in
  let pos_t = Utils.ty_of_int p.pos in
  let has_default_t = if p.has_default then tt else ff in
  [pos_t; name_t; has_default_t]

let extract_str ty =
  let open Sstt in
  match ty |> Ty.get_descr |> Descr.get_enums |> Enums.destruct with
    (true, [ name ]) -> name |> Enum.name
  | (b, l) -> fail "Invalid encoding of spec name (%b, %d)" b (List.length l)

let extract_int ty =
  let open Sstt in
  match Ty.get_descr ty
        |> Descr.get_intervals
        |> Intervals.destruct
        |> List.map Intervals.Atom.get
  with
    [ Some i, Some j ] when Z.equal i j -> Z.to_int i
  | _ -> failwith "Invalid encoding of spec position"

let tuple_get n t =
  let open Sstt in
  Tuples.get n (Descr.get_tuples (Ty.get_descr t))

let tuple_proj n i t =
  let open Sstt in
  tuple_get n t
  |> Op.TupleComp.proj i

let type_to_param is_pos rec_type pos name has_default =
  let open Sstt in
  let pos = pos |> extract_int in
  let name = name |> extract_str in
  let has_default = has_default |> Ty.leq tt in
  let field =
    if is_pos then Utils.field_name_pos pos
    else Utils.field_name_kw name
  in
  let ty = MT.Record.proj rec_type field  in
  { name; pos; ty; has_default }

let spec_by_id = Hashtbl.create 16
module TyHash = Hashtbl.Make(Sstt.Ty)
let id_by_spec = TyHash.create 16
let make_arg_spec_part (pb, ab, kb) =
  let pb_t = List.concat_map param_to_type pb |> MT.Tuple.mk in
  let ab_t = List.concat_map param_to_type ab |> MT.Tuple.mk in
  let kb_t = List.concat_map param_to_type kb |> MT.Tuple.mk in
  let spec = MT.Tuple.mk [pb_t; ab_t; kb_t] in
  let id = match TyHash.find_opt id_by_spec spec with
      Some e -> e
    | None -> let e = Sstt.Enum.mk "<ID>"  in
      Hashtbl.add spec_by_id e spec;
      TyHash.add id_by_spec spec e;
      e
  in
  let ty_id = Sstt.( id |> Descr.mk_enum |> Ty.mk_descr) in
  MT.Ty.cup dummy_t ty_id

let build builder =
  let tr = make_record_part builder |> Utils.mk_rec_disj false in
  let ts = make_arg_spec_part builder in
  [tr; ts]
  |> MT.Tuple.mk
  |> MT.Tag.mk tag

let is_non_empty_rec r =
  MT.Ty.leq r MT.Record.any &&
  not (MT.Ty.is_empty r) &&
  (MT.TVOp.top_vars r |> MT.TVarSet.is_empty)

let extract_record t =
  let ty = MT.Tag.proj tag t in
  let r = MT.Tuple.proj 2 0 ty in
  if is_non_empty_rec r then r
  else fail "Invalid record component"


let pack p pos kw =
  let pos_l, pos_e = List.mapi (fun i e -> Utils.field_name_pos i, e) pos |> List.split in
  let kw_l, kw_e = List.map (fun (s, e) -> Utils.field_name_kw s, e) kw |> List.split in
  let r = Utils.mk_record p (pos_l @ kw_l) (pos_e @ kw_e) in
  let e_dummy = Utils.mk_enum p dummy in
  let pair = Utils.mk_tuple p [r; e_dummy] in
  Utils.mk_tag p tag pair

let unpack p e =
  Utils.mk_proj_tag p tag e
  |> Utils.mk_proj_tuple p 2 0

let extract_single_tuple ty =
  let open Sstt in
  let tc = Ty.get_descr ty |> Descr.get_tuples |> Tuples.components in
  match tc with
    [], false -> []
  | [ tc ], false -> begin
      match Op.TupleComp.as_union tc with
        [ l ] -> l
      | _ -> fail "tuple type is not a single tuple"
    end
  | l, b -> fail "tuple type mix different arities %d, %b" (List.length l) b

let map_exact f (lp, la, lk) =
  let fp p = { p with ty = f p.ty } in
  List.(map fp lp, map fp la, map fp lk)

let map_approx f l =
  List.map (fun (pl, kl) ->
      List.map f pl,
      List.map (fun (s, t) -> s, f t) kl
    ) l

type 'ty descr =
    Exact of 'ty t
  | Approx of bool * ('ty list * (string * 'ty) list) list

let map f d = match d with
    Exact t -> Exact (map_exact f t)
  | Approx (b, l) -> Approx (b,map_approx f l)


let record_atom_to_sig node ctx r =
  let open Sstt in
  let open Records in
  let pos, kw =
    r.Atom.bindings
    |> LabelMap.to_list
    |> List.fold_left (fun (apos, akw) (l, (ty, opt)) ->
        assert (not opt);
        let ty = node ctx ty in
        match String.split_on_char '_' (Label.name l) with (* *)
        | ["p"; n] -> (int_of_string n, ty)::apos, akw
        | ["k"; n] -> apos, (n, ty)::akw
        | _ -> fail "Unexpected label %s" (Label.name l)
      ) ([], [])
  in
  List.(sort (fun (a,_) (b,_) -> Int.compare a b) pos |> map snd,
        sort (fun (a,_) (b,_) -> String.compare a b) kw)
let to_approx node ctx rec_type =
  let open Sstt in
  let sigs = Op.Records.as_union (rec_type |> Ty.get_descr |> Descr.get_records)  in
  let b = Ty.equiv rec_type
      (List.fold_left (fun acc a -> Ty.cup acc (Descr.mk_record a|>Ty.mk_descr)) Ty.empty sigs)
  in
  Approx (b, List.map (record_atom_to_sig node ctx) sigs)

let rec map3 f l =
  match l with
    [] -> []
  | x::y::z::ll -> (f x y z) :: map3 f ll
  | _ -> fail "Invalid list in map3"

let to_exact node ctx rec_type spec_id =
  let open Sstt in
  let id = Ty.diff spec_id dummy_t |> Ty.get_descr |> Descr.get_enums in
  let id = match Enums.destruct id with
      (true, [ id ]) -> id
    | _ -> fail "Invalid id"
  in
  let spec = Hashtbl.find spec_by_id id in
  let spec_pos = tuple_proj 3 0 spec |> extract_single_tuple in
  let spec_args = tuple_proj 3 1 spec |> extract_single_tuple in
  let spec_kw = tuple_proj 3 2 spec |> extract_single_tuple in
  let tr_param b ty1 ty2 ty3 =
    let p = type_to_param b rec_type ty1 ty2 ty3 in
    { p with ty = node ctx p.ty }
  in
  Exact ((map3 (tr_param true) spec_pos,
          map3 (tr_param true) spec_args,
          map3 (tr_param false) spec_kw))

let to_t node ctx comp : _ option =
  let open Sstt in
  let _, t = Op.TagComp.as_atom comp in
  let rec_type = MT.Tuple.proj 2 0 t in
  let spec = tuple_proj 2 1 t in
  Some (if Ty.leq spec dummy_t
        then to_approx node ctx rec_type
        else to_exact node ctx rec_type spec)

let pp_param fmt p =
  Format.fprintf fmt "%s=%s%a" p.name (if p.has_default then "?" else "")
    Sstt.Printer.print_descr p.ty

let print_exact _prec _assoc fmt (p, a, k) =
  let open Format in
  let p = match List.map (fun p -> `Spec p) p with
      [] -> []
    | l -> l @ [(`Str "/")]
  in
  let a = List.map (fun a -> `Spec a) a in
  let k = match List.map (fun k -> `Spec k) k with
      [] -> []
    | l -> (`Str "*") :: l
  in
  let pr fmt e = match e with
      `Spec p -> pp_param fmt p
    | `Str s -> pp_print_string fmt s
  in
  fprintf fmt "@[<hov 1>(%a)@]"
    (pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt ", ")
       pr) (p@a@k)

let pp_approx_sig fmt (lp, lkw) =
  let open Format in
  let open Sstt in
  let l = List.map Either.left lp in
  let l = l @ List.map Either.right lkw in
  let (s, _, _) = Prec.varop_info Tuple in
  fprintf fmt "@[(";
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "%s " s)
    (fun fmt e -> match e with
         Either.Left ty -> fprintf fmt "%a" Printer.print_descr ty
       | Either.Right (s, ty) -> fprintf fmt "%s=%a"s Printer.print_descr ty) fmt l;
  fprintf fmt ")@]"

let print_approx prec assoc fmt b l =
  let open Format in
  let open Sstt in
  let cup_info = Prec.(varop_info Cup) in
  let need_par = Prec.need_parentheses prec assoc cup_info in
  fprintf fmt "@[";
  if need_par then fprintf fmt "(";
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; " ) pp_approx_sig fmt l;
  if not b then (match l with [] -> fprintf fmt "..." | _ -> fprintf fmt "; ..." );
  if need_par then fprintf fmt ")";
  fprintf fmt "@]"

let print prec assoc fmt d =
  match d with
    Exact e -> print_exact prec assoc fmt e
  | Approx (b, l) -> print_approx prec assoc fmt b l

let py_params_builder : Sstt.Printer.extension_builder =
  Sstt.Printer.builder
    ~to_t
    ~map
    ~print

let params = Sstt.Printer.{ aliases = [];
                            extensions = [(MT.Tag.tag tag, py_params_builder) ]}

let () = MT.PEnv.add_printer_param params