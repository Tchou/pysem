open Aliases
open Utils

let builtins : (string, (MlVar.t * ML.Ast.t)) Hashtbl.t =
  (Hashtbl.create 16)

let find_opt s = Hashtbl.find_opt builtins s |> Option.map fst
let add ?(kind=MlMVar.Immut) vname body =
  let open Hashtbl in
  if mem builtins vname
  then failwith (vname ^ " already exists")
  else
    let v = mk_var_t ~kind vname in
    add builtins vname (v,body);
    v

let all () = builtins |> Hashtbl.to_seq_values |> List.of_seq
