module PyCo = PyreAst.Concrete
module PyTF = PyreAst.TaglessFinal


module IdentMap = Map.Make(PyreAst.Concrete.Identifier)
module IdentSet = Set.Make(PyreAst.Concrete.Identifier)
let dummy_pos = PyCo.Position.make_t ~line:~-1 ~column:~-1 ()
let dummy_loc = PyCo.Location.make_t ~start:dummy_pos ~stop:dummy_pos ()

let _builtins = [ (* Available via [dirs(__builtins__)] *)
  "ArithmeticError"; "AssertionError"; "AttributeError"; "BaseException"; "BaseExceptionGroup";
  "BlockingIOError"; "BrokenPipeError"; "BufferError"; "BytesWarning"; "ChildProcessError";
  "ConnectionAbortedError"; "ConnectionError"; "ConnectionRefusedError"; "ConnectionResetError";
  "DeprecationWarning"; "EOFError"; "Ellipsis"; "EncodingWarning"; "EnvironmentError"; "Exception";
  "ExceptionGroup"; "False"; "FileExistsError"; "FileNotFoundError"; "FloatingPointError";
  "FutureWarning"; "GeneratorExit"; "IOError"; "ImportError"; "ImportWarning"; "IndentationError";
  "IndexError";  "InterruptedError"; "IsADirectoryError"; "KeyError"; "KeyboardInterrupt"; "LookupError";
  "MemoryError"; "ModuleNotFoundError"; "NameError"; "None"; "NotADirectoryError"; "NotImplemented";
  "NotImplementedError";  "OSError"; "OverflowError"; "PendingDeprecationWarning"; "PermissionError";
  "ProcessLookupError"; "PythonFinalizationError"; "RecursionError"; "ReferenceError"; "ResourceWarning";
  "RuntimeError"; "RuntimeWarning"; "StopAsyncIteration"; "StopIteration"; "SyntaxError"; "SyntaxWarning";
  "SystemError"; "SystemExit"; "TabError"; "TimeoutError"; "True"; "TypeError"; "UnboundLocalError";
  "UnicodeDecodeError"; "UnicodeEncodeError"; "UnicodeError"; "UnicodeTranslateError"; "UnicodeWarning";
  "UserWarning"; "ValueError"; "Warning"; "ZeroDivisionError"; "_IncompleteInputError";
  "__build_class__"; "__debug__"; "__doc__"; "__import__"; "__loader__"; "__name__"; "__package__";
  "__spec__"; "abs"; "aiter"; "all"; "anext"; "any"; "ascii"; "bin"; "bool"; "breakpoint"; "bytearray";
  "bytes"; "callable"; "chr"; "classmethod"; "compile"; "complex"; "copyright"; "credits"; "delattr";
  "dict"; "dir"; "divmod"; "enumerate"; "eval"; "exec"; "exit"; "filter"; "float"; "format"; "frozenset";
  "getattr"; "globals"; "hasattr"; "hash"; "help"; "hex"; "id"; "input"; "int"; "isinstance"; "issubclass";
  "iter"; "len"; "license"; "list"; "locals"; "map"; "max"; "memoryview"; "min"; "next"; "object"; "oct";
  "open"; "ord"; "pow"; "print"; "property"; "quit"; "range"; "repr"; "reversed"; "round"; "set"; "setattr";
  "slice"; "sorted"; "staticmethod"; "str"; "sum"; "super"; "tuple"; "type"; "vars"; "zip"
]
module Error =
struct
  type t =
      IncompatibleScope of PyCo.Identifier.t * string * string
    | AltPatternNames (* patterns x | y *)
    | DuplicateArgument of PyCo.Identifier.t
    | UnboundNonlocal of PyCo.Identifier.t
  let to_pyre locations e =
    let open PyreAst.Parser.Error in
    let open Format in
    let loc = match locations with
        l :: _ -> l
      | _ -> dummy_loc
    in
    let mk_error message =
      let open PyCo.Location in
      { message;
        line = loc.start.line;
        column = loc.start.column;
        end_line = loc.stop.line;
        end_column = loc.stop.column;
      } in
    let id_str = PyCo.Identifier.to_string in
    match e with
    | IncompatibleScope (id1, s1, s2) ->
      mk_error (sprintf "Name '%s' has incompatible scope %s and %s"
                  (id_str id1) s1 s2)
    | AltPatternNames -> mk_error "Alternative pattern binds different names"
    | DuplicateArgument id ->
      mk_error (sprintf "Duplicate argument '%s'" (id_str id))
    | UnboundNonlocal id  ->
      mk_error (sprintf "Unbound nonlocal '%s'" (id_str id))
end
exception Error of Error.t * PyCo.Location.t list (* internal errors, re-raised see parse at the end of the file *)
let raise_ ?(locations=[]) e = raise (Error (e, locations))



type scope =
    Local | Parameter | Nonlocal | Global | Unknown


let show_scope = function
    Local -> "local"
  | Nonlocal -> "nonlocal"
  | Global -> "global"
  | Parameter -> "parameter"
  | Unknown -> "unknown (nonlocal or global)"

let pretty_scope = function
  | Local | Parameter -> "Ⓛ"
  | Nonlocal -> "Ⓝ"
  | Global -> "Ⓖ"
  | _ -> assert false

type context = { del : bool; load : bool; store : bool; }
let default_context = { del = false; load = false; store = false }

let context =
  PyCo.ExpressionContext.(
    function
      Del -> { default_context with del = true }
    | Load -> { default_context with load = true }
    | Store -> { default_context with store = true}
  )

type info =  {
  scope : scope;
  context : context;
  locations : PyCo.Location.t list
}

let ident ?location ?(scope=Unknown) ?(ctx=PyCo.ExpressionContext.make_load_of_t()) id =
  IdentMap.singleton id { scope; context = context ctx; locations = Option.to_list location }

let merge_scope locs1 locs2 var s1 s2 =
  match s1, s2 with
    Unknown, s | s, Unknown -> s
  | Parameter, Local | Local, Parameter -> Local
  | _ when s1 = s2 -> s1
  | _ ->
    let locations = locs1@ locs2 in
    raise_ ~locations (IncompatibleScope (var, show_scope s1, show_scope s2))

let merge_context c1 c2  = { del = c1.del || c2.del;
                             load = c1.load || c2.load;
                             store = c1.store || c2.store}
let merge_info var i1 i2 =
  { context = merge_context i1.context i2.context;
    scope = merge_scope i1.locations i2.locations var i1.scope i2.scope;
    locations = i1.locations @ i2.locations }
let merge_vars vars1 vars2 =
  IdentMap.union (fun var i1 i2 -> Some (merge_info var i1 i2)) vars1 vars2

let lambda_name = "<LAMBDA>"
let lambda_id = PyCo.Identifier.make_t lambda_name ()
type block_kind = Fun | AsyncFun | Lambda | Class | Module

module BlockId =
struct
  type t =
    { kind : block_kind;
      name : PyCo.Identifier.t;
      location : PyCo.Location.t }
  let mk ~kind ~name ~location = { kind; name=PyCo.Identifier.make_t name (); location }
  let mk_fun    name location = mk ~name ~location ~kind:Fun
  and mk_afun   name location = mk ~name ~location ~kind:AsyncFun
  and mk_lambda      location = mk ~name:lambda_name ~location ~kind:Lambda
  and mk_class  name location = mk ~name ~location ~kind:Class
  and mk_module name location = mk ~name ~location ~kind:Module

  let hash = Hashtbl.hash
  let equal f1 f2 =
    f1.kind = f2.kind &&
    PyCo.Identifier.compare f1.name f2.name = 0 &&
    PyCo.Location.compare f1.location f2.location = 0

end
module BidTable = Hashtbl.Make(BlockId)
module IdentTable = Hashtbl.Make(struct include PyCo.Identifier let equal a b = compare a b = 0 end)
type env = {
  blocks : (info IdentMap.t * BlockId.t list) BidTable.t;
  mutable globals : info IdentMap.t;
  filename : string
}
let create_env filename =
  { blocks = BidTable.create 16;
    globals = IdentMap.empty;
    filename
  }

(* Computing a node of the AST can either yield the node itself,
   for any construct that cannot contain an identifier
   (e.g. operators).
   Otherwise, it is a triple of:
   1. the AST node
   2. the map of identifiers from the current scope in the node
   3. the list of block identifiers from the current scope in the node
*)
type 'a result = 'a * info IdentMap.t * BlockId.t list

let get1 ((a, _, _) : 'a result) = a
let get2 ((_, a, _) : 'a result) = a
let get3 ((_, _, a) : 'a result) = a

let store_ctx = PyCo.ExpressionContext.make_store_of_t ()
(*
  [bind]/[bind_opt] must be used for identifiers that bind names, see:
 https://docs.python.org/3/reference/executionmodel.html#binding-of-names
*)
let bind ~location name =
  name,
  ident ~location ~ctx:store_ctx name,
  []
let bind_opt ~location o = match o with
    None -> None, IdentMap.empty, []
  | Some name ->
    let r = bind ~location name in
    (Some (get1 r)), get2 r, get3 r

(* Computations of free variables and variable scope *)

let enter_arguments scope (a : PyCo.Arguments.t) vars =
  let open PyCo.ExpressionContext in
  let seen = IdentTable.create 16 in
  let add_arg_list l vars =
    List.fold_left (fun avars PyCo.Argument.{location;identifier; _} ->
        (* same effect as bind above *)
        if IdentTable.mem seen identifier then raise_ ~locations:[location] (DuplicateArgument (identifier));
        IdentTable.add seen identifier ();
        ident ~location ~scope ~ctx:(make_store_of_t ()) identifier
        |> merge_vars avars
      ) vars l
  in
  vars
  |> add_arg_list a.posonlyargs
  |> add_arg_list a.args
  |> add_arg_list (Option.to_list a.vararg)
  |> add_arg_list a.kwonlyargs
  |> add_arg_list (Option.to_list a.kwarg)

(* Computes the variables used in a block.
   Scoping requires two passes:
   - while building the parse tree, we mark variables that
     are known Local, Nonlocal or Global
   - variable that remain unknown are resolved in a second pass, using
     LEGB (local, enclosing, global, builtins)
*)
let compute_block_variables env location kind name args body =
  let vars = List.fold_left (fun acc (_,v, _) -> merge_vars acc v) IdentMap.empty body in
  let idents = List.concat_map get3 body in

  let vars = enter_arguments Parameter args vars in
  let nvars = vars |> IdentMap.map (fun info ->
      let scope =
        match info.scope with
        | Unknown when info.context.del || info.context.store ->
          if kind = Module then Global else Local
        | s -> s
      in
      { info with scope }
    )
  in
  let bid = BlockId.{ kind; name; location } in
  BidTable.add env.blocks bid (nvars, idents);
  List.map get1 body, ident ~location ~scope:Unknown ~ctx:store_ctx name, [bid]


(* Tweaked version of Pyre's own AST builder,
   see:
   https://github.com/grievejia/pyre-ast/blob/trunk/lib/concrete.ml
*)

(* Custom indexing to reduce boiler plate (a bit). See :
   https://ocaml.org/manual/5.3/bindingops.html
*)

(* Applied last in a sequence of let/and. [vars] is the accumulated mappings of
   all the subsequent [and*] and [l] their accumulated id list. Just execute the
    in part on all the accumulated arguments.
*)
(* Regular binding, merge their components *)

let (let*) (a, (vars:info IdentMap.t), (l : BlockId.t list)) f = (f a), vars, l
let (and*) (a, b, c) (d, e, f) = (a, d), merge_vars b e, c @ f

(* to ensure that the first intermediate result has the form:
   'a * 'b IdentMap.t * 'c list, other wise we need to handle the case of first
   thing being a list/option/list of option, option of lists, etc…
*)
let _init = (), IdentMap.empty, []

(* Thread the option monad *)

let (and*?) (a, b, c) o =
  match o with
    None ->   (a, None), b, c
  | Some (d,e,f) -> (a, Some d), merge_vars b e, c @ f

(* Thread the list monad *)
let (and*@) (a, b, c) l =
  let l, v, i =
    List.fold_left (fun (al, av, ai) (el, ev, ei) ->
        el::al, merge_vars av ev, ai @ ei
      ) ([], b, c) l
  in (a, List.rev l), v, i

(* Thread the option monad inside the list monad (list of options)*)
let (and*?@) (a, b, c) l =
  let l, v, i =
    List.fold_left (fun (al, av, ai) ol ->
        match ol with
          None -> (None::al, av, ai)
        | Some (el,ev,ei) -> (Some el)::al, merge_vars av ev, ai @ ei
      ) ([], b, c) l
  in (a, List.rev l), v, i


let _init = (), IdentMap.empty, []


(* AST Nodes, in order of:
   https://grievejia.github.io/pyre-ast/doc/pyre-ast/PyreAst/TaglessFinal/index.html
*)
let constant =
  let open PyCo.Constant in
  PyTF.Constant.make ~none:(make_none_of_t ()) ~false_:(make_false_of_t ())
    ~true_:(make_true_of_t ()) ~ellipsis:(make_ellipsis_of_t ()) ~integer:make_integer_of_t
    ~big_integer:make_big_integer_of_t ~float_:make_float_of_t ~complex:make_complex_of_t
    ~string_:make_string_of_t ~byte_string:make_byte_string_of_t ()

let expression_context =
  let open PyCo.ExpressionContext in
  PyTF.ExpressionContext.make ~load:(make_load_of_t ()) ~store:(make_store_of_t ())
    ~del:(make_del_of_t ()) ()

let boolean_operator =
  let open PyCo.BooleanOperator in
  PyTF.BooleanOperator.make ~and_:(make_and_of_t ()) ~or_:(make_or_of_t ()) ()

let binary_operator =
  let open PyCo.BinaryOperator in
  PyTF.BinaryOperator.make ~add:(make_add_of_t ()) ~sub:(make_sub_of_t ())
    ~mult:(make_mult_of_t ()) ~matmult:(make_matmult_of_t ()) ~div:(make_div_of_t ())
    ~mod_:(make_mod_of_t ()) ~pow:(make_pow_of_t ()) ~lshift:(make_lshift_of_t ())
    ~rshift:(make_rshift_of_t ()) ~bitor:(make_bitor_of_t ()) ~bitxor:(make_bitxor_of_t ())
    ~bitand:(make_bitand_of_t ()) ~floordiv:(make_floordiv_of_t ()) ()

let comparison_operator =
  let open PyCo.ComparisonOperator in
  PyTF.ComparisonOperator.make ~eq:(make_eq_of_t ()) ~noteq:(make_noteq_of_t ())
    ~lt:(make_lt_of_t ()) ~lte:(make_lte_of_t ()) ~gt:(make_gt_of_t ())
    ~gte:(make_gte_of_t ()) ~is:(make_is_of_t ()) ~isnot:(make_isnot_of_t ())
    ~in_:(make_in_of_t ()) ~notin:(make_notin_of_t ()) ()

let unary_operator =
  let open PyCo.UnaryOperator in
  PyTF.UnaryOperator.make ~invert:(make_invert_of_t ()) ~not_:(make_not_of_t ())
    ~uadd:(make_uadd_of_t ()) ~usub:(make_usub_of_t ()) ()

let comprehension ~target ~iter ~ifs ~is_async =
  let* target and* iter and*@ ifs in
  PyCo.Comprehension.make_t ~target ~iter ~ifs ~is_async ()

let keyword ~location ~arg ~value =
  (* The arg identifier in a keyword is not binding, and does not interact with the namespace
     def f():
      global x
      g(x=42) <- x and the global x variable are distinct
  *)
  let* value in
  PyCo.Keyword.make_t ~location ?arg ~value ()

let check_duplicate_keyword l =
  let cmp (a : PyCo.Keyword.t) (b : PyCo.Keyword.t) =
    Option.compare PyCo.Identifier.compare a.arg b.arg
  in
  let l = List.sort cmp l in
  let rec loop l =
    match l with
      [ ] | [ _ ] -> ()
    | a1 :: (a2 :: _ as ll) ->
      if cmp a1 a2 = 0 then
        raise_ ~locations:[a1.location;a2.location] (DuplicateArgument (Option.get a1.arg))
      else loop ll
  in
  loop l

let argument ~location ~identifier ~annotation ~type_comment =
  let* _init and*? annotation in
  PyCo.Argument.make_t ~location ~identifier ?annotation ?type_comment ()

let arguments ~posonlyargs ~args ~vararg ~kwonlyargs ~kw_defaults ~kwarg ~defaults  =
  let* _init and*@ posonlyargs
  and*@ args and*? vararg
  and*@ kwonlyargs and*?@ kw_defaults
  and*? kwarg and*@ defaults in
  PyCo.Arguments.make_t ~posonlyargs ~args ?vararg ~kwonlyargs ~kw_defaults ?kwarg ~defaults ()

(* Expressions: auxiliary definitions corresponding to constructors of the Concrete.Expressioon.t are
   local functions *)

let expression tbl =
  let open PyCo.Expression in
  let bool_op ~location ~op ~values =
    let* _init and*@ values in
    make_boolop_of_t ~location ~op ~values ()
  in
  let named_expr ~location ~target ~value =
    (* target := expr. target is guaranteed to be a variable name. Nothing special to do here,
       the name case below handles the target, for which the parser will put the Store context
    *)
    let* target and* value in
    make_namedexpr_of_t ~location ~target ~value ()
  in
  let bin_op ~location ~left ~op ~right =
    let* left and* right in
    PyCo.Expression.make_binop_of_t ~location ~left ~op ~right ()
  in
  let unary_op ~location ~op ~operand =
    let* operand in
    make_unaryop_of_t ~location ~op ~operand ()
  in
  let lambda ~location ~args ~body =
    let* _init and* args
    and* body = compute_block_variables tbl location Lambda lambda_id (get1 args) [body] in
    make_lambda_of_t ~location ~args ~body:(List.hd body) ()
  in
  let if_exp ~location ~test ~body ~orelse =
    let* test and* body and* orelse in
    make_ifexp_of_t ~location ~test ~body ~orelse ()
  in
  let dict ~location ~keys ~values =
    let* _init and*?@ keys and*@ values in
    make_dict_of_t ~location ~keys ~values ()
  in
  let set ~location ~elts =
    let* _init and*@ elts in
    make_set_of_t ~location ~elts ()
  in
  let list_comp ~location ~elt ~generators =
    let* elt and*@ generators in
    make_listcomp_of_t ~location ~elt ~generators ()
  in
  let set_comp ~location ~elt ~generators =
    let* elt and*@ generators in
    make_setcomp_of_t ~location ~elt ~generators ()
  in
  let dict_comp ~location ~key ~value ~generators =
    let* key and* value and*@ generators in
    make_dictcomp_of_t ~location ~key ~value ~generators ()
  in
  let generator_exp ~location ~elt ~generators =
    let* elt
    and*@ generators in
    make_generatorexp_of_t ~location ~elt ~generators ()
  in
  let await ~location ~value =
    let* value in
    make_await_of_t ~location ~value ()
  in
  let yield ~location ~value =
    let* _init
    and*? value in
    make_yield_of_t ~location ?value ()
  in
  let yield_from ~location ~value =
    let* value in
    make_yieldfrom_of_t ~location ~value ()
  in
  let compare ~location ~left ~ops ~comparators =
    let* left
    and*@ comparators in
    make_compare_of_t ~location ~left ~ops ~comparators ()
  in
  let call ~location ~func ~args ~keywords =
    let* func and*@ args and*@ keywords in
    check_duplicate_keyword keywords;
    make_call_of_t ~location ~func ~args ~keywords ()
  in
  let formatted_value ~location ~value ~conversion ~format_spec =
    let* value and*? format_spec in
    make_formattedvalue_of_t ~location ~value ~conversion ?format_spec ()
  in
  let joined_str ~location ~values =
    let* _init and*@ values in
    make_joinedstr_of_t ~location ~values ()
  in
  let constant ~location ~value ~kind =
    make_constant_of_t ~location ~value ?kind (), IdentMap.empty, []
  in
  let attribute ~location ~value ~attr ~ctx =
    (* Here the context is for the field access [a.b] itself, so pass it along. *)
    let* value in
    make_attribute_of_t ~location ~value ~attr ~ctx ()
  in
  let subscript ~location ~value ~slice ~ctx =
    let* value and* slice in
    make_subscript_of_t ~location ~value ~slice ~ctx ()
  in
  let starred ~location ~value ~ctx =
    let* value in make_starred_of_t ~location ~value ~ctx ()
  in
  let name ~location ~id ~ctx =
    let* id = id, ident ~location ~ctx id, [] in
    make_name_of_t ~location ~id ~ctx ()
  in
  let list ~location ~elts ~ctx =
    let* _init and*@ elts in
    make_list_of_t ~location ~elts ~ctx ()
  in
  let tuple ~location ~elts ~ctx =
    let* _init and*@ elts in
    make_tuple_of_t ~location ~elts ~ctx ()
  in
  let slice ~location ~lower ~upper ~step =
    let* _init and*? lower and*? upper and*? step in
    make_slice_of_t ~location ?lower ?upper ?step ()
  in
  PyTF.Expression.make
    ~bool_op ~named_expr ~bin_op ~unary_op ~lambda ~if_exp ~dict ~set
    ~list_comp ~set_comp ~dict_comp ~generator_exp ~await ~yield ~yield_from ~compare
    ~call ~formatted_value ~joined_str ~constant ~attribute ~subscript ~starred ~name
    ~list ~tuple ~slice ()

let with_item ~context_expr ~optional_vars =
  let* context_expr and*? optional_vars in
  PyCo.WithItem.make_t ~context_expr ?optional_vars ()

let import_alias ~location ~name ~asname =
  match asname with
    None ->
    let* name = bind ~location name in
    PyCo.ImportAlias.make_t ~location ~name ?asname ()
  | Some n ->
    let* asname = bind ~location n in
    PyCo.ImportAlias.make_t ~location ~name ?asname:(Some asname) ()

let type_param =
  let type_var ~location ~name ~bound =
    let* name = bind ~location name
    and*? bound in
    PyCo.TypeParam.make_typevar_of_t ~location ~name ?bound ()
  in
  let param_spec ~location ~name =
    let* name = bind ~location name in
    PyCo.TypeParam.make_paramspec_of_t ~location ~name ()
  in
  let type_var_tuple ~location ~name =
    let* name = bind ~location name in
    PyCo.TypeParam.make_typevartuple_of_t ~location ~name ()
  in
  PyTF.TypeParam.make ~type_var ~param_spec ~type_var_tuple ()

let exception_handler ~location ~type_ ~name ~body =
  let* _init
  and*? type_
  and* name = bind_opt ~location name
  and*@ body in
  PyCo.ExceptionHandler.make_t ~location ?type_ ?name ~body ()

let match_case ~pattern ~guard ~body =
  let* pattern
  and*? guard
  and*@ body in
  PyCo.MatchCase.make_t ~pattern ?guard ~body ()

let pattern =
  let open PyCo.Pattern in
  (* See : https://peps.python.org/pep-0622/#allowed-patterns *)
  let match_value ~location ~value =
    let* value in
    make_matchvalue_of_t ~location ~value ()
  in
  let match_singleton ~location ~value =
    make_matchsingleton_of_t ~location ~value (), IdentMap.empty, []
  in
  let match_sequence ~location ~patterns =
    let* _init and*@ patterns in
    make_matchsequence_of_t ~location ~patterns ()
  in
  let match_class ~location ~cls ~patterns ~kwd_attrs ~kwd_patterns =
    let* cls
    and*@ patterns
    and*@ kwd_patterns in
    make_matchclass_of_t ~location ~cls ~patterns ~kwd_attrs ~kwd_patterns ()
  in
  let match_mapping ~location ~keys ~patterns ~rest =
    (* keys are  guaranteed to be constants so there should be no identifier with a del or store context inside *)
    let* rest = bind_opt ~location rest and*@ keys and*@ patterns  in
    make_matchmapping_of_t ~location ~keys ~patterns ?rest ()
  in
  let match_star ~location ~name =
    let* name = bind_opt ~location name in
    make_matchstar_of_t ~location ?name ()
  in
  let match_as ~location ~pattern ~name =
    let* _init
    and*? pattern
    and* name = bind_opt ~location name in (* case _ is None, None, case x: is None, Some x, case x as _ is rejected by the parser *)
    make_matchas_of_t ~location ?pattern ?name ()
  in
  let match_or ~location ~patterns =
    let () =  (* detect patterns with bad domain *)
      match patterns with
        [] -> ()
      | (_,v,_) :: l ->
        if not (List.for_all (fun (_,v',_) ->
            IdentMap.equal (fun _ _ -> true) v v' ) l)
        then
          raise_ AltPatternNames
    in
    let* _init and*@ patterns in
    make_matchor_of_t ~location ~patterns ()
  in
  PyTF.Pattern.make ~match_value ~match_singleton ~match_sequence ~match_mapping ~match_class ~match_star ~match_as ~match_or ()


(* Statements *)
let statement tbl =
  let open PyCo.Statement in
  let mk_fun mk kind ~location ~name ~args ~body ~decorator_list ~returns ~type_comment ~type_params =
    let* _init
    and* args
    and* body = compute_block_variables tbl location kind name (get1 args) body
    and*@ decorator_list
    and*? returns
    and*@ type_params
    in
    mk ~location ~name ~args ?body:(Some body) ?decorator_list:(Some decorator_list)
      ?returns ?type_comment ?type_params:(Some type_params) ()
  in
  let function_def = mk_fun make_functiondef_of_t Fun in
  let async_function_def = mk_fun make_functiondef_of_t AsyncFun in
  let class_def ~location ~name ~bases ~keywords ~body ~decorator_list ~type_params =
    let* _init
    and*@ bases
    and*@ keywords
    and* body = compute_block_variables tbl location Class name (PyCo.Arguments.make_t()) body
    and*@ decorator_list
    and*@ type_params in
    check_duplicate_keyword keywords;
    make_classdef_of_t ~location ~name ~bases ~keywords ~body ~decorator_list ~type_params ()
  in
  let return ~location ~value =
    let* _init and*? value in
    make_return_of_t ~location ?value ()
  in
  let delete ~location ~targets =
    let* _init and*@ targets in make_delete_of_t ~location ~targets ()
  in
  let assign ~location ~targets ~value ~type_comment =
    let* _init
    and*@ targets
    and* value in
    make_assign_of_t ~location ~targets ~value ?type_comment ()
  in

  let type_alias ~location ~name ~type_params ~value =
    let* name
    and*@ type_params
    and* value in
    make_typealias_of_t ~location ~name ~type_params ~value ()
  in
  let aug_assign ~location ~target ~op ~value =
    let* target
    and* value in
    make_augassign_of_t ~location ~target ~op ~value ()
  in
  let ann_assign ~location ~target ~annotation ~value ~simple =
    (* see https://docs.python.org/3/reference/simple_stmts.html#annassign
       for the treatment of simple assignments
    *)
    let* target =
      if not simple then target else
        let id_expr,vars, bids = target in
        let id = match id_expr with
            PyCo.Expression.Name{id; _} -> id
          | _ -> assert false
        in
        let info = IdentMap.find id vars in
        id_expr, IdentMap.add id {info with scope=Local} vars, bids
    and* annotation
    and*? value in
    make_annassign_of_t ~location ~target ~annotation ?value ~simple ()

  in
  let mk_for mk ~location ~target ~iter ~body ~orelse ~type_comment =
    let* target and* iter and*@ body and*@ orelse in
    mk ~location ~target ~iter ?body:(Some body) ?orelse:(Some orelse) ?type_comment ()
  in
  let for_ = mk_for make_for_of_t in
  let async_for = mk_for make_asyncfor_of_t in
  let mk_while mk ~location ~test ~body ~orelse =
    let* test and*@ body and*@ orelse in
    mk ~location ~test ?body:(Some body) ?orelse:(Some orelse) ()
  in
  let while_ = mk_while make_while_of_t in
  let if_ = mk_while make_if_of_t in
  let mk_with mk ~location ~items ~body ~type_comment =
    let* _init and*@ items and*@ body in
    mk ~location ?items:(Some items) ?body:(Some body) ?type_comment ()
  in
  let with_ = mk_with make_with_of_t in
  let async_with = mk_with make_asyncwith_of_t in
  let match_ ~location ~subject ~cases =
    let* subject and*@ cases in
    make_match_of_t ~location ~subject ~cases ()
  in
  let raise_ ~location ~exc ~cause =
    let* _init and*? exc and*? cause in
    make_raise_of_t ~location ?exc ?cause ()
  in
  let mk_try mk ~location ~body ~handlers ~orelse ~finalbody =
    let* _init and*@ body and*@ handlers and*@ orelse and*@ finalbody in
    mk ~location ?body:(Some body) ?handlers:(Some handlers) ?orelse:(Some orelse) ?finalbody:(Some finalbody) ()
  in
  let try_ = mk_try make_try_of_t in
  let try_star = mk_try make_trystar_of_t in
  let assert_ ~location ~test ~msg =
    let* test and*? msg in
    make_assert_of_t ~location ~test ?msg ()
  in
  let import ~location ~names =
    let* _init and*@ names in make_import_of_t ~location ~names ()
  in
  let import_from ~location ~module_ ~names ~level =
    let* _init and*@ names in
    make_importfrom_of_t ~location ?module_ ~names ~level ()
  in
  let mk_scope mk scope ~location ~names =
    let vars = List.fold_left (fun acc n ->
        IdentMap.add n {scope; context=default_context;locations=[location]} acc)
        IdentMap.empty names
    in mk ~location ?names:(Some names) (), vars, []
  in
  let global = mk_scope make_global_of_t Global in
  let nonlocal = mk_scope make_nonlocal_of_t Nonlocal in
  let expr ~location ~value =
    let* value in make_expr_of_t ~location ~value ()
  in
  PyTF.Statement.make ~function_def ~async_function_def ~class_def ~return ~delete ~assign ~type_alias
    ~aug_assign ~ann_assign ~for_ ~async_for ~while_ ~if_ ~with_ ~async_with ~match_ ~raise_ ~try_  ~try_star
    ~assert_ ~import ~import_from ~global ~nonlocal ~expr
    ~pass:(fun ~location -> make_pass_of_t ~location (), IdentMap.empty, [])
    ~break:(fun ~location -> make_break_of_t ~location (), IdentMap.empty, [])
    ~continue:(fun ~location -> make_continue_of_t ~location (), IdentMap.empty, [])
    ()

let type_ignore ~lineno ~tag = PyCo.TypeIgnore.make_t ~lineno ~tag ()

let function_type ~argtypes ~returns =
  let* _init
  and*@ argtypes
  and* returns in PyCo.FunctionType.make_t ~argtypes ~returns ()


(* Modules are the entrypoint of parsing *)
type block_info = {
  name : string;
  filename : string;
  location : PyCo.Location.t;
  kind : block_kind;
  identifiers : info IdentMap.t;
  defines : (string * PyCo.Location.t * block_kind) list
}
let pp_loc fmt (loc : PyCo.Location.t) =
  if PyCo.Location.compare dummy_loc loc <> 0 then
    Format.fprintf fmt "%d:%d-%d:%d"
      loc.start.line
      loc.start.column
      loc.stop.line
      loc.stop.column

let show_block_kind = function
  | Lambda -> "lambda"
  | Fun -> "function"
  | AsyncFun -> "coroutine"
  | Class -> "class"
  | Module -> "module"

let pp_block_kind fmt k =
  let s = show_block_kind k in
  Format.fprintf fmt "%s" s

let pp_info fmt i = Format.fprintf fmt "%s,(del=%b,load=%b,store=%b)"
    (show_scope i.scope) i.context.del i.context.load i.context.store

let pp_vars fmt vars =
  let open Format in
  fprintf fmt "%a"
    (pp_print_list ~pp_sep:pp_print_space
       (fun fmt (v, i) -> fprintf fmt "%s=%a" (PyCo.Identifier.to_string v) pp_info i))
    vars

let pp_defines fmt (s, loc, k) =
  Format.fprintf fmt "%s (%a) %a" s pp_loc loc pp_block_kind k
let pp_block_info fmt bi =
  let open Format in
  fprintf fmt "@[%a %s (%s:%a)@]@\n" pp_block_kind bi.kind
    bi.name
    bi.filename
    pp_loc bi.location;
  fprintf fmt "@[vars:@[<v>";
  pp_vars fmt (IdentMap.bindings bi.identifiers);
  fprintf fmt "@]@]@\n";
  fprintf fmt "@[defines:@[<v>";
  pp_print_list ~pp_sep:pp_print_space pp_defines fmt bi.defines;
  fprintf fmt "@]@]@\n--"

let rec resolve_unknown_scope enclosing scope (tbl : env) bid =
  let vars, bids = BidTable.find tbl.blocks bid in
  let r_vars = IdentMap.filter_map (fun var infos ->
      match infos.scope with
      | Nonlocal when not (IdentSet.mem var enclosing) ->
        raise_ ~locations:infos.locations (UnboundNonlocal var)
      | (Global|Nonlocal) when not infos.context.load &&
                               not infos.context.store &&
                               not infos.context.del -> None
      (* variable where referenced in nonlocal or global but never used *)

      | Unknown ->
        if scope = Nonlocal && IdentSet.mem var enclosing then
          Some { infos with scope = Nonlocal }
        else
          let () = tbl.globals <- IdentMap.add var infos tbl.globals in
          Some { infos with scope = Global }
      | Global ->
        let () = tbl.globals <- IdentMap.add var infos tbl.globals in
        Some infos
      | _ ->  Some infos
    ) vars
  in
  BidTable.replace tbl.blocks bid (r_vars, bids);
  let nscope, nenclosing = match bid.kind with
      Module -> Global, enclosing
    | Fun|AsyncFun|Lambda ->
      Nonlocal,
      IdentMap.fold (fun var infos acc ->
          if infos.scope = Local then IdentSet.add var acc else acc)
        r_vars enclosing
    | Class -> scope, enclosing
  in
  {name = PyCo.Identifier.to_string bid.BlockId.name;
   filename = tbl.filename;
   location = bid.BlockId.location;
   kind = bid.BlockId.kind;
   identifiers = r_vars;
   defines = List.map (fun bid ->
       BlockId.(PyCo.Identifier.to_string bid.name, bid.location, bid.kind)) bids;
  } :: List.concat_map (resolve_unknown_scope nenclosing nscope tbl) bids


let module_gen (tbl:env) ~body ~type_ignores =
  let module_name = PyCo.Identifier.make_t tbl.filename () in
  let body, vars, bids = compute_block_variables tbl dummy_loc Module
      module_name (PyCo.Arguments.make_t ~args:[] ()) body
  in
  PyCo.Module.make_t ~body ~type_ignores (),vars, bids

let module_ (tbl:env) ~body ~type_ignores =
  let m, v, i = module_gen tbl ~body ~type_ignores in
  assert (IdentMap.cardinal v = 1 && List.compare_length_with i 1 = 0); (* The module name *)
  m,resolve_unknown_scope IdentSet.empty Global tbl (List.hd i)

(* after we are done, any remaining variable that has scope Local is changed
   to Global (it is "local" to the module) and any free variable that remains should raise an error.
*)

let spec env =
  PyTF.make ~argument ~arguments ~binary_operator ~boolean_operator
    ~comparison_operator ~comprehension ~constant
    ~exception_handler
    ~expression:(expression env)
    ~expression_context
    ~function_type
    ~identifier:(fun s -> PyCo.Identifier.make_t s ())
    ~import_alias
    ~keyword
    ~location:(fun ~start ~stop -> PyCo.Location.make_t ~start ~stop ())
    ~match_case
    ~module_:(module_ env)
    ~pattern
    ~position:(fun ~line ~column -> PyCo.Position.make_t ~line ~column ())
    ~statement:(statement env)
    ~type_ignore
    ~type_param
    ~unary_operator
    ~with_item ()

exception Syntax of string * PyreAst.Parser.Error.t

let syntax file e = raise (Syntax (file, e))

let parse ~file =
  let str = In_channel.(with_open_text file input_all) in
  let env = create_env file in
  let open PyreAst in
  match
    Parser.with_context (fun context ->
        Parser.
          TaglessFinal.parse_module ~context ~spec:(spec env) ~enable_type_comment:true str
      )
  with
    Error e -> syntax file e
  | exception Error (e, locations) -> syntax file (Error.to_pyre locations e)
  | Ok (ast,defs) -> ast, env.globals, defs, Utils.loc_converter file str
