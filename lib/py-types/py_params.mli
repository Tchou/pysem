(** Python argument specifications *)
open Aliases


(** {1 Building argument specifications for function signatures} *)

type 'ty builder
(** The type of a builder of argument specification *)

val empty : 'ty builder
(** The empty builder *)

val add_param : 'ty builder -> [`Pos|`Arg|`Kwd] -> int -> string -> 'ty -> bool -> 'ty builder
(** [add_param b k pos name v has_default ] adds an argument to builder [b].

    - [k] denotes the type of arguments ([`Pos]itional only, mixed [`Arg]ument, [`Kwd]-only argument)
    - [pos] is the position of the argument in the parameter list,
    - [name] is the name of the argument in the parameter list
    - [v] is the data associated with this argument
    - [has_default] is [true] if the argument has a default value
*)

val build : Sstt.Ty.t builder -> Sstt.Ty.t
(** [build b] creates an opaque type which represents a list of argument specifications. *)

val extract_record : Sstt.Ty.t -> Sstt.Ty.t
(** [extract_record t] returns the record encoding part of the argument specificaiton list *)

(** {1 Argument packing and access} *)

val pack : MC.Position.t -> MLAst.t list -> (string * MLAst.t) list -> MLAst.t
(** [pack pos pos_args kw_args] creates a term that represent all Python
    arguments packed before a function call. For the call [f(1,2,3,x=4,y=5)],
    this function should be called with [pack pos [a1,a2,a3] [ "x",a4; "y",a5
    ]].
*)

val unpack : MC.Position.t -> MLAst.t -> MLAst.t
(** [unpack pos arg ] creates a term that extracts the record part of the encoded argument list.
    For a function call [f(1,2,3,x=4,y=5)] was packed with [pack pos [a1,a2,a3] [ "x",a4; "y",a5]],
    [unpack pos arg] returns the AST of a record: [ { __1=1; __2=2; __3=3; x=4; y=5 } ] (
    the name of the labels are given as an example. The actual name can be obtained with
    {!field_name_pos} and {!field_name_kw}).
*)

val field_name_pos : int -> string
(** [field_name_pos i] builds the record field name corresponding to a positional argument in postition [i]. *)

val field_name_arg : int -> string -> string
(** [field_name_arg i s] builds a record field name for a mixed argument at position [i] with name [s]. *)

val field_name_kw : string -> string
(** [field_name_kw s] builds the record field name corresponding to a keyword argument [s]. *)

val getter_pk_name : string -> string

val getter_a_name : int -> string -> bool -> string

(** {1 Pretty-printing}

   This module registers a pretty-printer for python type signatures, which are
   printed using Python or pylint conventions. For the function [f] below:
   {[
    def f (x, y,/,z=42, *, u=12):
      ...
   ]}
    its signature is printed as:
    {[
      (x:tx, y:tx, /, z:tz = ..., *, u:tu = ... ) -> tres
    ]}
    The string [= ...] is fixed (and a valid Python expression using the elipsis constant)
*)

val pp_py_scheme : Format.formatter -> MT.TyScheme.t -> unit
(** [pp_py_scheme fmt s] prints the toplevel type-scheme s as a Python signature.
    For the function [f] below:
    {[
        def f (a, b, c = 42):
          return (a, b, c+3)
    ]}
    its signature is printed as:
    {[
      ['X, 'Y](a:'X, b:'Y, c: int = ... ) -> ('X,'Y,int)
    ]}
    A heuristic is used to rename variables of the type scheme.
    From 1 to 3 variables, the names are ['X], ['Y] and ['Z].
    For mor that 3 variables, the names are ['X1, …, 'Xn].
*)