open Aliases

val pr : string -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [pr title format] prints [title] in cyan bold, [":"] and then acts like
    [Printf.printf format].
 *)

val dbg_pr : string -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [dbg title format] acts like [pr title format] but only prints if
    [Utils.debug] is set at [false], and prints title in blue.
 *)

val pp_list :
  ?sep:(unit,Format.formatter,unit) format ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit
(** [pp_list] acts like [Format.pp_print_list]. *)

val pp_nel : string -> 'a list -> string
(** [pp_nel s l] prints s if l is not empty. Useful for separators only present
    if a list will be showned.
 *)

val mlvar_show : MlVar.t -> string
val mlvar_show_full : MlVar.t -> string

module MSAstPrinter : sig
  val pp_variable : Format.formatter -> MlVar.t -> unit
  val pp_projection : Format.formatter -> MSAst.projection -> unit

  open MSAst
  val pp_e : Format.formatter -> e -> unit
  val pp_t : Format.formatter -> t -> unit
  val err_pp : MC.Eid.t -> Format.formatter -> t -> unit
end
(** Mlsem_system.Ast.t printer **)


module MLAstPrinter : sig
  val pp_variable : Format.formatter -> MlVar.t -> unit
  val pp_projection : Format.formatter -> MSAst.projection -> unit

  open MLAst
  val pp_pattern_constructor : Format.formatter -> pattern_constructor -> unit
  val pp_pattern : Format.formatter -> pattern -> unit
  val pp_e : Format.formatter -> e -> unit
  val pp_t : Format.formatter -> t -> unit
end
(** Mlsem_lang.Ast.t printer **)

module PAstPrinter : sig
  val pp_projection : Format.formatter -> MSAst.projection -> unit

  open Mlsem_app
  val pp_pattern : Format.formatter -> ('a, 'b ,'c, 'd, string) PAst.pattern -> unit
  val pp_ast : Format.formatter -> ('a, 'b, 'c, 'd, 'e, string) PAst.ast -> unit
  val pp_t : Format.formatter -> ('a, 'b ,'c, 'd, 'e,  string) PAst.t -> unit
end
(** Mlsem_app.Past.t printer **)

val pp_ml_top : Format.formatter -> MlVar.t * MLAst.t -> unit
val pp_ml_tys : Format.formatter -> MlVar.t * MT.TyScheme.t -> unit
