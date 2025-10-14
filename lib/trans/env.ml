open Parsing

type t =
  { current : block_info
  ; infos : block_info BidTable.t
  ; filename : string
  ; to_loc : Utils.loc_converter
  }

let init bil to_loc =
  let infos = BidTable.create 16 in
  let filename = ref "<MODULE>" in
  List.iter (fun ({name; location; kind; _ } as bi) ->
      BidTable.add infos (BlockId.mk ~name ~location ~kind) bi;
      match kind with
      | Module -> filename := name
      | _ -> ()
    ) bil;
  { current = BidTable.find infos
                (BlockId.mk_module !filename Parsing.dummy_loc)
  ; infos
  ; filename = !filename
  ; to_loc
  }
