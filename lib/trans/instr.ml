open Mlsem_app
open Mlsem
open Ast

module PC = PyreAst.Concrete

let zip_for l1 l2 =
  let rec loop l1 l2 acc =
    match l1, l2 with
    | [], l2 -> acc, l2
    | e1 :: ll1, e2 :: ll2 -> loop ll1 ll2 ((e1, Some e2)::acc)
    | e1 :: ll1, []        -> loop ll1 []  ((e1, None   )::acc)
  in
  loop l1 l2 []

let rec of_statement env (stmt:PC.Statement.t) : instr =
  let annot = env_annot env in
  match stmt with
  | FunctionDef r ->
     let fenv = (* new scope → new environement *)
       let open Parsing in
       let bi = BlockId.mk_fun (PC.Identifier.to_string r.name) r.location in
       { env with current = BidTable.find env.infos bi } in
     let a : PC.Arguments.t = r.args in
     let args, rem_args = zip_for (List.rev a.args) (List.rev a.defaults) in
     let posonly, rem_pos = zip_for (List.rev a.posonlyargs) rem_args in
     assert (rem_pos = []);
     let kwonly = List.combine a.kwonlyargs a.kw_defaults in

     let format (arg,eo) = Ident.of_argument fenv arg
                         , Option.map (Expr.of_expression fenv) eo in

     let body = List.map (of_statement fenv) r.body in

     FunDef ( Ident.of_identifier env r.name
            , { posonly = List.map format posonly
              ; args    = List.map format args
              ; kwonly  = List.map format kwonly
              ; vararg  = Option.map (Ident.of_argument fenv) a.vararg
              ; kwarg   = Option.map (Ident.of_argument fenv) a.kwarg }
            , body )
     |> annot r.location
  (* | AsyncFunctionDef | ClassDef *)
  | Return r -> Return (Option.map (Expr.of_expression env) r.value)
                |> annot r.location
  (* | Delete *)
  | Assign r ->
     let el = List.map (Expr.of_expression env) r.targets in
     let e = Expr.of_expression env r.value in
     Assign (el,e) |> annot r.location
  (* | TypeAlias | AugAssign | AnnAssign | For | AsyncFor *)
  | While ({orelse=[];_} as r) ->
     let e = Expr.of_expression env r.test in
     let is = List.map (of_statement env) r.body in
     While (e,Block is |> annot r.location) |> annot r.location
  | While _ -> failwith "Not implemented (Instr.While(orelse))"
  | If r ->
     let test = Expr.of_expression env r.test in
     let thn = Block (List.map (of_statement env) r.body  ) |> annot r.location in
     let els = match List.map (of_statement env) r.orelse with
       | [] -> Option.None
       | l -> Some (Block l |> annot r.location) in
     If (test,thn,els) |> annot r.location
  (* | With | AsyncWith | Match | Raise | Try | TryStar | Assert | Import
     | ImportFrom  *)
  | Global {location;_} | Nonlocal {location;_} | Pass {location}
    -> Block [] |> annot location (* or Expr.None ? *)
  | Expr r -> Iexpr (Expr.of_expression env r.value) |> annot r.location
  | Break {location} -> annot location Break
  | Continue {location} -> annot location Continue
  | _ -> failwith "Not implemented (Instr)."

(* === === === === === === *)

let dummy_def = PAst.Definitions []
let dummy_ast = PAst.Tuple []
let dummy_exp = (PAst.new_annot Common.Position.dummy, dummy_ast)

let translate_stmt _env _stmt : PAst.parser_expr = failwith "TODO"

let translate_top env stmt : PAst.(annotation * parser_element) =
  let ast_to_t loc ast = env.Env.to_loc loc |> PAst.new_annot, ast in
  let open PC.Statement in
  match stmt with
  | FunctionDef r ->
     (* def f(a,b=bdef,*,k=kdef):
            ...
        ↓

        let f = (* or let env = {env with f = … } *)
          let b_ = [bdef] in
          let k_ = [kdef] in
          let f = fun r ->
            let a = r.#0 in
            let b = (r∈{#1;..})? r.#1 : (r∈{b;..})?r.b:b_ in
            let k = (r∈{ k;..})? r.k  : k_ in
            [...]
          in
          f
      *)

     let mlarg_str = "%#rec_arg" in
     let mlarg = PAst.(Var mlarg_str |> ast_to_t r.location) in
     let mk_var loc id =
       PAst.Var (PC.Identifier.to_string id) |> ast_to_t loc
     and mk_lambda loc var body =
       PAst.Lambda (var, None, body) |> ast_to_t loc
     and mk_let loc id expin exp =
       (* TODO monadic: update environement *)
       PAst.Let ( (Immut, PC.Identifier.to_string id)
                , expin
                , exp) |> ast_to_t loc
     and _mk_ite loc test ty thn els =
       PAst.Ite (test, ty, thn, els) |> ast_to_t loc
     in
     let load_args (p_args:Arguments.proto) body =
       let add_varinit expr (a,i,_eo,_tv) =
         let loc = a.PC.Argument.location in
         mk_let loc
           a.PC.Argument.identifier
           PAst.(Projection (Field (Arguments.pos_param_name i), mlarg)
                 |> ast_to_t loc)
           expr
       in
       let rpos, _rargs, _rkw = List.( rev p_args.pos_only
                                     , rev p_args.args
                                     , rev p_args.kw_only) in
       List.fold_left add_varinit body rpos
     and fun_def loc f_id preamble_and_body =
       mk_let loc
         f_id
         (mk_lambda loc mlarg_str preamble_and_body)
         (mk_var loc f_id)
     and default_init _p_args f_def : PAst.parser_expr =
       f_def
     in
     let body = dummy_exp in
     let proto = Arguments.translate_arguments r.args in
     let annot = PAst.new_annot (env.Env.to_loc r.location) in
     let expr = default_init proto
                  (fun_def r.location r.name
                     (load_args proto body)) in
     let defs = [( (PAst.Immut, PC.Identifier.to_string r.name)
                 , expr)] in
     let () = Format.printf "Function %s: @[%a@]@\n"
                (PC.Identifier.to_string r.name)
                Arguments.pp_proto proto in
     let () = Format.printf "result:@.  @[%a@]@\n%!"
                Ast.PAstPrinter.pp_t expr in
     (annot, PAst.Definitions defs)
  | _ -> PAst.new_annot Common.Position.dummy, dummy_def
