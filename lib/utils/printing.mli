open Aliases

val pr : string -> ('a, Format.formatter, unit, unit) format4 -> 'a
val dbg_pr : string -> ('a, Format.formatter, unit, unit) format4 -> 'a

val pp_list :
  ?sep:string -> (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit
val pp_nel : string -> 'a list -> string

val mlvar_show : MlVar.t -> string

module MSAstPrinter : sig
  val pp_variable : Format.formatter -> MlVar.t -> unit
  val pp_projection : Format.formatter -> MSAst.projection -> unit

  open MSAst
  val pp_e : Format.formatter -> e -> unit
  val pp_t : Format.formatter -> t -> unit
end

module MLAstPrinter : sig
  val pp_variable : Format.formatter -> MlVar.t -> unit
  val pp_projection : Format.formatter -> MSAst.projection -> unit

  open MLAst
  val pp_pattern_constructor : Format.formatter -> pattern_constructor -> unit
  val pp_pattern : Format.formatter -> pattern -> unit
  val pp_e : Format.formatter -> e -> unit
  val pp_t : Format.formatter -> t -> unit
end

module PAstPrinter : sig
  val pp_projection : Format.formatter -> MSAst.projection -> unit

  open Mlsem_app
  val pp_pattern : Format.formatter -> ('a, 'b ,'c, string) PAst.pattern -> unit
  val pp_ast : Format.formatter -> ('a, 'b, 'c, 'd, string) PAst.ast -> unit
  val pp_t : Format.formatter -> ('a, 'b ,'c, 'd, string) PAst.t -> unit
end

val pp_ml_top : Format.formatter -> MlVar.t * MLAst.t -> unit
val pp_ml_tys : Format.formatter -> MlVar.t * MT.TyScheme.t -> unit
