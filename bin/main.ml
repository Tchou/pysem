open Pysem
let usage_message = Format.sprintf "%s <file.py>" Sys.argv.(0)
let input_file = ref None

let add_input_file s = 
  match !input_file with 
    None -> input_file := (Some s)
  | Some _ -> raise (Arg.Bad "multiple files provided")

let options = Arg.align []


let main () =
  Arg.parse options add_input_file usage_message;
  match !input_file with
    None ->
    Format.eprintf "%s: missing file@\n%s" Sys.argv.(0) 
      (Arg.usage_string options usage_message)
  | Some file ->
    Parsing.parse ~file


let () = try main () with
  | Error.Syntax (file, e) -> 
    Format.eprintf "%s: %d:%d-%d%d : %s@\n" 
      file e.line e.column e.end_line e.end_column e.message;
    exit 3 
  | Error.Unimplemented (file, l) ->
    Format.eprintf "%s: %d:%d-%d%d : Unimplemented construct@\n" 
      file l.start.line l.start.column l.stop.line l.stop.column;
    exit 3 

  | Sys_error msg -> Format.eprintf "%s@\n" msg; exit 1
  | e  -> Format.eprintf "ERROR: %s@\n" (Printexc.to_string e); exit 10
