open Parsing

type t = {
  infos : block_info BidTable.t;
  filename : string;
  to_loc : Utils.loc_converter;
}

let init bil to_loc =
  let infos = BidTable.create 16 in
  let filename = ref "<MODULE>" in
  List.iter (fun ({name; location; kind; _ } as bi) ->
      BidTable.add infos (BlockId.mk ~name ~location ~kind) bi;
      match kind with
        Module -> filename := name
      | _ -> ()
    ) bil;
  { infos; filename = !filename ; to_loc }