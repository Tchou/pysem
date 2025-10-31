open Parsing
open Utils.Aliases

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
                 ( MlVar.create (Some (PCI.to_string ident))
                 , info )
                 vmap )
             current.identifiers
             IdentMap.empty
  ; to_loc }
