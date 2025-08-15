
type t = { 
  idents : (string * unit) list;
  filename : string;
  start_of_lines : int array;  (* to convert to Lexing.positions *)
}

let init filename content = {
  idents = [];
  filename;
  start_of_lines = 
    (0 :: (String.split_on_char '\n' content |> List.map String.length)) 
    |> Array.of_list
}
let add name v env = { env with idents = (name, v) :: env.idents }
let line_offset env l =
  if l >= 0 && l < Array.length env.start_of_lines then
    env.start_of_lines.(l)
  else
    invalid_arg (Format.sprintf "line_offset: File %s, invalid line number: %d\n" env.filename l)