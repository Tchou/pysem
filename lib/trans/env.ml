open Parsing
open Aliases

type t =
  { current : block_info
  ; infos : block_info BidTable.t
  ; filename : string
  ; vars : (MlVar.t * info) IdentMap.t
  ; to_loc : Utils.loc_converter }

let top_kind = ref MlMVar.Immut
let local_kind = ref MlMVar.Mut

module Vartbl = Hashtbl.Make (struct
    type t = MlVar.t
    let equal = MlVar.equal
    let hash = Hashtbl.hash
  end)

type var_info =
  { block_def : BlockId.t ;
    in_blocks : BlockId.t list ;
    ty_var : Sstt.Var.t ;
    ty : Sstt.Ty.t }

let variables : var_info Vartbl.t = Vartbl.create 16
let vartbl_add mlvar (block_def:BlockId.t) def =
  match Vartbl.find_opt variables mlvar with
  | None ->
    let ty_var = MT.TVar.(Some (MlVar.show mlvar) |> mk KInfer) in
    Vartbl.add variables mlvar
      {block_def; in_blocks=[block_def]; ty_var; ty = MT.TVar.typ ty_var}
  | Some vi ->
    Vartbl.replace variables mlvar
      { vi with block_def = if def then block_def else vi.block_def ;
                in_blocks = block_def::vi.in_blocks }
and get_var_infos = Vartbl.find variables
and get_vars_infos () = Vartbl.fold
    (fun mlv infos acc -> (mlv,infos)::acc) variables []
and vars_rec b =
  Vartbl.fold
    (fun mlvar vi acc ->
       (MlVar.show mlvar, (vi.ty, b))::acc)
    variables
    []

let set_mut_top b = top_kind := MlMVar.(if b then Mut else Immut)
let set_mut_local b = local_kind := MlMVar.(if b then Mut else Immut)

let init globals bil to_loc =
  let infos = BidTable.create 16 in
  let mod_id,filename =
    List.fold_left (fun m ({name; location; kind; _ } as bi) ->
        let bid = BlockId.mk ~name ~location ~kind in
        BidTable.add infos bid bi;
        match kind with
          | Module -> Some (bid,name)
          | _ -> m
      ) None bil
    |> function
    | None -> raise Not_found
    | Some (bid,name) -> bid,name in
  { current = BidTable.find infos mod_id
  ; infos
  ; filename
  ; vars = IdentMap.fold
        (fun ident info vmap ->
           let var = Utils.mk_var_t ~kind:!top_kind (PCI.to_string ident) in
           vartbl_add var mod_id true; (* or module name *)
           IdentMap.add ident (var, info) vmap)
        globals
        IdentMap.empty
  ; to_loc }

let upd env bi =
  let open Parsing in
  let lift_scope = function Local b -> Nonlocal b | s -> s in
  let current = BidTable.find env.infos bi in
  let vars =
    env.vars
    |> IdentMap.map (fun (mlv, info) ->
        (mlv, { info with scope = lift_scope info.scope }))
    |> IdentMap.fold
      ( fun py_ident py_info vmap ->
          let var, def = match py_info.scope with
            | Local _ ->
              Utils.mk_var_t ~kind:!local_kind (PCI.to_string py_ident), true
            | Nonlocal _ | Global -> IdentMap.find py_ident env.vars |> fst
                                   , false
            | Unknown -> assert false in
          vartbl_add var bi def;
          IdentMap.add py_ident (var, py_info) vmap )
      current.identifiers
  in
  { env with current; vars }
