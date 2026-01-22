open Parsing
open Aliases

type t =
  { current : block_info
  ; infos : block_info BidTable.t
  ; filename : string
  ; vars : (MlVar.t * info * int) IdentMap.t
  ; module_id : BlockId.t
  ; to_loc : Utils.loc_converter }

let init globals bil to_loc =
  let infos = BidTable.create 16 in
  let filename = ref "" in
  List.iter (fun ({name; location; kind; _ } as bi) ->
      BidTable.add infos (BlockId.mk ~name ~location ~kind) bi;
      match kind with
      | Module -> filename := name
      | _ -> ()
    ) bil;
  let module_id = BlockId.mk_module !filename Parsing.dummy_loc in
  let current = BidTable.find infos module_id in
  { current
  ; infos
  ; filename = !filename
  ; vars = IdentMap.fold
        (fun ident info vmap ->
           IdentMap.add
             ident
             ( Utils.mk_var_t ~kind:MlMVar.Immut (PCI.to_string ident)
             , info
             , current.id )
             vmap )
        globals
        IdentMap.empty
  ; module_id
  ; to_loc }

let find_scope py_ident nonlocals =
  match List.find_opt (fun (_ , set) ->
      Parsing.IdentSet.mem py_ident set
    ) nonlocals
  with
    None -> 0
  | Some (l, _) -> l
let upd env bi =
  let open Parsing in
  let current = BidTable.find env.infos bi in
  let vars =
    IdentMap.fold
      (fun py_ident py_info vmap ->
         IdentMap.add py_ident
           (match py_info.scope with
            | Local | Parameter ->
              Utils.mk_var_t ~kind:MlMVar.Mut (PCI.to_string py_ident),
              py_info,
              current.id
            | Nonlocal | Global ->
              IdentMap.find py_ident env.vars |> (fun (x,_,_) -> x),
              py_info,
              find_scope py_ident current.nonlocals
            | Unknown -> assert false
           )
           vmap)
      current.identifiers env.vars in
  { env with current; vars }
