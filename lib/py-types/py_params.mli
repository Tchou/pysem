(** Python argument specifications *)
open Aliases

type 'ty t
(** The type of a builder of argument specification *)

val empty : 'ty t

val add_param : 'ty t -> [`Pos|`Arg|`Kwd] -> int -> string -> 'ty -> bool -> 'ty t
(** [add_param b k pos name v has_default ] adds an argument to builder [b].

    - [k] denotes the type of arguments ([`Pos]itional onlty, mixed [`Arg]ument, [`Kwd]-only argument)
    - [pos] is the position of the argument in the parameter list,
    - [name] is the name of the argument in the parameter list
    - [v] is the data associated with this argument
    - [has_default] is [true] if the argument has a default value
*)

val build : Sstt.Ty.t t -> Sstt.Ty.t
(** [build b] creates an opaque type which represents a list of argument specifications.
*)


val extract_record : Sstt.Ty.t -> Sstt.Ty.t
(** [extract_record t] returns the record encoding part of the argument specificaiton list *)

val pack : MC.Position.t -> MLAst.t list -> (string * MLAst.t) list -> MLAst.t
(** [pack pos pos_args kw_args] creates a term that represent all Python arguments packed
    before a function call.
*)

val unpack : MC.Position.t -> MLAst.t -> MLAst.t
(** [unpack pos arg ] creates a term that extracts the record part of the encoded argument list*)