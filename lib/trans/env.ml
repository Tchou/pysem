open Parsing
open Aliases

type t =
  { current : block_info
  ; infos : block_info BidTable.t
  ; filename : string
  ; vars : (MlVar.t * info) IdentMap.t
  ; to_loc : Utils.loc_converter }

let init bil to_loc =
  let infos = BidTable.create 16 in
  let filename = ref "<MODULE>" in
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
               IdentMap.add
                 ident
                 ( Utils.mk_var_t ~kind:MlMVar.Immut (PCI.to_string ident)
                 , info )
                 vmap )
             current.identifiers
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
               Utils.mk_var_t ~kind:MlMVar.Mut (PCI.to_string py_ident)
            | Nonlocal | Global ->
               IdentMap.find py_ident env.vars |> fst
            | Unknown -> assert false
            end
          , py_info)
          vmap)
      current.identifiers env.vars in
  { env with current; vars }
