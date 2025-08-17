open PyreAst.Concrete

exception Syntax of string * PyreAst.Parser.Error.t

(** Exception that encapsulate PyreAst errors as well as custom errors raised
    during variable analysis. *)

module IdentMap : Map.S with type key = Identifier.t
(** Maps indexed by variable names *)

type scope = Local | Parameter | Nonlocal | Global | Unknown
(** The scope of an identifier *)

type context = { del : bool; load : bool; store : bool; }
(** The expression context of an identifier  *)

type info = {
  scope : scope;
  context : context;
  locations : Location.t list;
}
(** Informations about an identifier: scope, context and locations of its occurrences. *)


type block_kind = Fun | AsyncFun | Lambda | Class | Module
(** The kind of a block *)

type block_info = {
  name : string;                 (** name: for a module, the filename, for a lambda the string ["<LAMBDA>"] *)
  filename : string;             (** filename *)
  location : Location.t;         (** location of the block in the file *)
  kind : block_kind;             (** the kind of the block *)
  identifiers : info IdentMap.t; (** a map of identifers defined in the scope of the block *)
  defines : (string * Location.t * block_kind) list; (** name, location and kind of the blocks defined in this one. *)
}
(** Informations about blocks *)

val pp_loc : Format.formatter -> Location.t -> unit
(** Pretty print a location, or nothing if the location is a dummy one. *)

val pp_block_info : Format.formatter -> block_info -> unit
(** Pretty print block informations. *)

val parse : file:string -> Module.t * block_info list
(** [parse ~file] returns a pair [(ast, bil)] where [ast] is the concrete AST of
    the module defined written in [file] and [bil] is the list of all scope blocks
    defined in the file.

    @raise Syntax if a syntax error occurs.
*)