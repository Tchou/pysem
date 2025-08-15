open Mlsem_lang


let mk_annot (env : Env.t) (loc : PyreAst.Concrete.Location.t) =
  let open Common.Position in

  let pos_fname = env.filename in
  let start_offset = Env.line_offset env loc.start.line in
  let stop_offset = Env.line_offset env loc.stop.line in
  with_poss
    Lexing.{pos_fname; 
            pos_lnum = loc.start.line;
            pos_bol = start_offset;
            pos_cnum = start_offset + loc.start.column;
           }
    Lexing.{pos_fname; 
            pos_lnum = loc.stop.line;
            pos_bol = stop_offset;
            pos_cnum = stop_offset + loc.stop.column;
           } 
    () |> position |> PAst.new_annot

