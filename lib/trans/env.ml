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

let variables = Vartbl.create 16
let vartbl_add mlvar scope_name =
  let tv = MT.TVar.(Some (MlVar.show mlvar) |> mk KInfer) in
  Vartbl.add variables mlvar
    (scope_name, tv, MT.TVar.typ tv)
and get_var_infos = Vartbl.find variables
and vars_rec b =
  Vartbl.fold
    (fun mlvar (_, _, typ) acc ->
       (MlVar.show mlvar, (typ, b))::acc)
    variables []

let set_mut_top b = top_kind := MlMVar.(if b then Mut else Immut)
let set_mut_local b = local_kind := MlMVar.(if b then Mut else Immut)

let init globals bil to_loc =
  let infos = BidTable.create 16 in
  let filename = ref "" in
  List.iter (fun ({name; location; kind; _ } as bi) ->
      BidTable.add infos (BlockId.mk ~name ~location ~kind) bi;
      match kind with
      | Module -> filename := name
      | _ -> ()
    ) bil;
  let current = BidTable.find infos
      (BlockId.mk_module !filename Parsing.dummy_loc) in
  { current
  ; infos
  ; filename = !filename
  ; vars = IdentMap.fold
        (fun ident info vmap ->
           let var = Utils.mk_var_t ~kind:!top_kind (PCI.to_string ident) in
           vartbl_add var "global"; (* or module name *)
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
          let var = match py_info.scope with
            | Local _ ->
              Utils.mk_var_t ~kind:!local_kind (PCI.to_string py_ident)
            | Nonlocal _ | Global -> IdentMap.find py_ident env.vars |> fst
            | Unknown -> assert false in
          vartbl_add var current.name;
          IdentMap.add py_ident (var, py_info) vmap )
      current.identifiers
  in
  { env with current; vars }
