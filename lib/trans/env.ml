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
  let current = BidTable.find infos (BlockId.mk_module !filename Parsing.dummy_loc) in
  { current
  ; infos
  ; filename = !filename
  ; vars = IdentMap.fold
        (fun ident info vmap ->
           IdentMap.add
             ident
             ( Utils.mk_var_t ~kind:!top_kind (PCI.to_string ident)
             , info )
             vmap )
        globals
        IdentMap.empty
  ; to_loc }

let upd env bi =
  let open Parsing in
  let current = BidTable.find env.infos bi in
  let vars =
    IdentMap.fold
      ( fun py_ident py_info vmap ->
          IdentMap.add py_ident
            ( begin match py_info.scope with
                | Local | Parameter ->
                  Utils.mk_var_t ~kind:!local_kind (PCI.to_string py_ident)
                | Nonlocal | Global -> IdentMap.find py_ident env.vars |> fst
                | Unknown -> assert false
              end
            , py_info)
            vmap)
      current.identifiers env.vars in
  { env with current; vars }
