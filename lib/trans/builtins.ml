open Mlsem.Types

(*
The base construct for the object hierarchy is open records
each record has a __dict__ field and a __class__ field

when looking for a property, it is looked for in 
__dict__, if absent, it is looked for in
__class__.__mro__ in order

val attribute_error : string -> empty

let get_x_from_mro mro =
  match mro with
  [] -> attribute_error "x"
  | cls::mro -> 
    if cls is { __dict__ : { x : any .. } .. } then (cls.__dict__).x else
    get_x_from_mro mro
 end

 let get_x o = 
  if o is { __dict__ : { x : any .. } .. } then (o.__dict__).x else
  get_x_from_mro ((o.__class__).__mro__)

*)

let base : Builder.type_expr =
  let open TyExpr in
  TRecord (true, [])