open Aliases
open Utils
open Ast

let rec of_statement env (stmt:PC.Statement.t) : instr =
  let annot = env_annot env in
  match stmt with
  | FunctionDef r ->
     let fenv = Parsing.BlockId.mk_fun (PCI.to_string r.name) r.location
                |> Env.upd env in
     let spec = Expr.spec_of_arguments fenv r.args in
     let body = Block (List.map (of_statement fenv) r.body)
                |> annot r.location in
     FunDef ( Ident.of_identifier env r.name
            , spec
            , body )
     |> annot r.location
  (* | AsyncFunctionDef | ClassDef *)
  | Return r -> Return (Option.map (Expr.of_expression env) r.value)
                |> annot r.location
  (* | Delete *)
  | Assign ({targets=[Name {id=x;_}];_} as r) ->
     let x = Ident.of_identifier env x in
     let e = Expr.of_expression env r.value in
     Assign (x,e) |> annot r.location
  | Assign _ -> failwith "Not implemented (Instr.Assign(¬ only 1 target var))."
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

  | AsyncFunctionDef _ | ClassDef _ | Delete _ | TypeAlias _ | AugAssign _
    | AnnAssign _ | For _ | AsyncFor _ | With _ | AsyncWith _ | Match _
    | Raise _ | Try _ | TryStar _ | Assert _ | Import _ | ImportFrom _
    -> failwith "Not implemented (Instr)."


let no_var ast = dummy_var_t, ast

let rec to_ml (p,instr:instr) : (MlMVar.t * MLAst.t) =
  match instr with
  | Block l ->
     begin match List.fold_left (fun acc i ->
                     match i with
                     | _, Block [] -> acc
                     | _, Block [i] | i -> to_ml i::acc)
                   [] l
     with
     | [] -> assert false
     | (v,e)::r as l ->
        let l, last = if v = dummy_var_t then r,e else l,dummy_ml_ast in
        join_let_rev l last |> no_var
     end
  | FunDef (f, args, body) ->
     ( f.name
     , Expr.ml_lambda p args (to_ml body |> snd) )
  | Return eo ->
     ( match eo with None -> mk_unit p | Some e -> Expr.to_ml e )
     |> mk_return p |> no_var
  | Assign (x,e) -> (x.name, Expr.to_ml e)
  | While _ -> failwith "TODO while"
  | If _ -> failwith "TODO if"
  | Iexpr e -> Expr.to_ml e |> no_var
  | Break | Continue -> mk_break p |> no_var
