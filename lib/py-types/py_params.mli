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
    arguments packed before a function call. For the call [f(e1,e2,e3,x=e4,y=e5)],
    this function should be called with [pack pos [a1,a2,a3] [ "x",a4; "y",a5
    ]] where [ai] is the ASTs of expression [ei].
*)

val unpack : MC.Position.t -> MLAst.t -> MLAst.t
(** [unpack pos arg] creates a term that extracts the record part of the encoded arguments.
    For a function call [f(e1,e2,e3,x=e4,y=e5)] was packed with [pack pos [a1,a2,a3] [ "x",a4; "y",a5]],
    [unpack pos arg] returns the AST of a record: [ { __1=a1; __2=a2; __3=a3; x=a4; y=a5 } ] (
    the name of the labels are an implementation detail. The parameters should be extracted with
    {!positional_getter}, {!argument_getter} or {!kwonly_getter}.
*)

val positional_getter : MC.Position.t -> int -> MLAst.t -> MLAst.t option -> MLAst.t
(** [positional_getter pos i arg odef] returns an AST that extracts the record
    field corresponding to the ith positional parameter of ther record [arg].
    If [odef] is [None], the field must be present. Otherwise, if [odef] is [Some def],
    then [def] is returned if the field is absent.
*)

val argument_getter : MC.Position.t -> int -> string -> MLAst.t -> MLAst.t option -> MLAst.t
(** [argument_getter pos i kw arg odef] returns an AST that extracts the record
  field corresponding to regular parameter in the ith position or of name
  [kw] record [arg]. If [odef] is [None], the field must be present.
  Otherwise, if [odef] is [Some def], then [def] is returned if the field is
  absent.
*)

val kwonly_getter : MC.Position.t -> string -> MLAst.t -> MLAst.t option -> MLAst.t
(** [kwonly_getter pos kw arg odef] returns an AST that extracts the record
    field corresponding to a keyword only parameter [kw] of ther record [arg].
    If [odef] is [None], the field must be present. Otherwise, if [odef] is [Some def],
    then [def] is returned if the field is absent.
*)

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
(** [pp_py_scheme fmt s] prints the toplevel type-scheme [s] as a Python signature.
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