
exception Syntax of string * PyreAst.Parser.Error.t

(** Exception that encapsulate PyreAst errors as well as custom errors raised
    during variable analysis. *)

module IdentMap : Map.S with type key = PyreAst.Concrete.Identifier.t
(** Maps indexed by variable names *)

type scope = Local | Parameter | Nonlocal | Global | Unknown
(** The scope of an identifier *)

type context = { del : bool; load : bool; store : bool; }
(** The expression context of an identifier  *)

type info = {
  scope : scope;
  context : context;
  locations : PyreAst.Concrete.Location.t list;
}
(** Informations about an identifier: scope, context and locations of its occurrences. *)



type block_kind = Fun | AsyncFun | Lambda | Class | Module
(** The kind of a block *)

module BlockId : sig
  type t
  val mk : kind:block_kind -> name:string -> location:PyreAst.Concrete.Location.t -> t
  val mk_fun : string -> PyreAst.Concrete.Location.t -> t
  val mk_afun : string -> PyreAst.Concrete.Location.t -> t
  val mk_lambda : string -> PyreAst.Concrete.Location.t -> t
  val mk_class : string -> PyreAst.Concrete.Location.t -> t
  val mk_module : string -> PyreAst.Concrete.Location.t -> t
  val equal : t -> t -> bool
  val hash : t -> int
end
module BidTable : Hashtbl.S with type key = BlockId.t

type block_info = {
  name : string;                 (** name: for a module, the filename, for a lambda the string ["<LAMBDA>"] *)
  filename : string;             (** filename *)
  location : PyreAst.Concrete.Location.t;         (** location of the block in the file *)
  kind : block_kind;             (** the kind of the block *)
  identifiers : info IdentMap.t; (** a map of identifers defined in the scope of the block *)
  defines : (string * PyreAst.Concrete.Location.t * block_kind) list; (** name, location and kind of the blocks defined in this one. *)
}
(** Informations about blocks *)

val dummy_loc : PyreAst.Concrete.Location.t
(** Dummy location, used for modules and builtins variables. **)

val pp_loc : Format.formatter -> PyreAst.Concrete.Location.t -> unit
(** Pretty print a location, or nothing if the location is a dummy one. *)

val show_block_kind : block_kind -> string
val pp_block_info : Format.formatter -> block_info -> unit
(** Pretty print block informations. *)

val pp_vars : Format.formatter -> (PyreAst.Concrete.Identifier.t * info) list -> unit

val parse : file:string -> PyreAst.Concrete.Module.t * block_info list * Utils.loc_converter
(** [parse ~file] returns a pair [(ast, bil)] where [ast] is the concrete AST of
    the module defined written in [file] and [bil] is the list of all scope blocks
    defined in the file.

    @raise Syntax if a syntax error occurs.
*)
