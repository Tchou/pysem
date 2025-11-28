open Aliases
open Utils

let builtins : (string, (MlVar.t * ML.Ast.t)) Hashtbl.t =
  (Hashtbl.create 16)

let add vname body =
  let open Hashtbl in
  if mem builtins vname
  then find builtins vname |> fst
  else
    let v = mk_var_t ~kind:MlMVar.Immut vname in
    add builtins vname (v,body);
    v

let getter_pk field =
  let open Hashtbl in
  let gname = getter_pk_name field in
  if mem builtins gname
  then find builtins gname |> fst
  else let g = mk_getter_pk field in
       let v = mk_var_t ~kind:MlMVar.Immut gname in
       add builtins gname (v,g);
       v
and getter_a i k fp fk fa d =
  let open Hashtbl in
  let gname = getter_a_name i k d in
  if mem builtins gname
  then find builtins gname |> fst
  else let g = (if d then mk_getter_a_d else mk_getter_a) fp fk fa in
       let v = mk_var_t ~kind:MlMVar.Immut gname in
       add builtins gname (v,g);
       v
