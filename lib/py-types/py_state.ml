open Aliases

type res_kind = R | V
type ident = Simple of MlVar.t
           | Source of Ast.ident
type lambda_kind =
  | Normal of Sstt.Ty.t | State of Sstt.Ty.t * (ident list) (* list of locals *)
  | Both of Sstt.(Ty.t * ident * Ty.t * ident * (Ty.t * (ident list)))

let is_admin = function Normal _ -> false | _ -> true
type expr' =
  | Const of Ast.const
  | Var of ident
  | Res of res_kind * expr
  | Proj of res_kind * expr
  | IfV of {cond:expr; v:ident; s:ident; body:expr}
  | IfR of {cond:expr; v:ident; s:ident; body:expr}
  | Ite of expr * expr * expr
  | Tuple of expr list
  | Pi of int * expr
  | EmptyRec
  | RecUpdate of expr * ident * expr (* { e1 with id = e2 } *)
  | Field of expr * ident
  | DelStateFields of ident list * expr
  | Lambda of ident * lambda_kind * expr (* true ⇒ synthetic *)
  | App of expr * expr
  | Cast of expr * MlGTy.t
and expr = MC.Position.t * expr'

(* Config variables *)
type ps_config =
  { get_cast : Parsing.scope -> bool (* cast variable get access if… *)
  ; set_cast : Parsing.scope -> bool (* cast variable set access if… *)
  ; mk_state : Ast.scoped_identifiers -> MT.Ty.t
  (* returns the Sstt.Ty.t of the state when entering a scope. *)
  ; end_state_cast : expr -> MT.Ty.t -> ident list -> expr
  (* cast the returned state in lambdas & fundef *)
  ; fun_pair : bool (* def f(x):... to λ(x,s). and not λs.λx. *)
  ; init_state : Ast.IdentSet.t -> MT.Ty.t
  ; pack_toplevel : bool (* used by Main: 1 let toplevel or a sequence? *)
  ; cast_toplevel : bool (* used by Main: cast each toplevel statement? *)
  }

let test_cfg : ps_config =
  let get_cast sc = Parsing.(match sc with Nonlocal _ -> true | _ -> false)
  and set_cast sc = Parsing.(match sc with Nonlocal _ -> true | _ -> false)
  and mk_state (sid:Ast.scoped_identifiers) =
    Env.(Vartbl.fold
           (fun mlvar (scope_name,_,_) l ->
              let ty =
                let open Ast.IdentSet in
                let tv =
                  (* _ty *)
                  Printing.mlvar_show mlvar |> Utils.mk_tv
                in
                if scope_name = Env.module_scope
                then tv
                else if mem_ml mlvar sid.nl_used
                then MT.Ty.conj [tv; MT.Ty.neg Utils.undef]
                else MT.Ty.any in
              (MlVar.show mlvar, (ty, false))::l)
           variables [])
    |> MT.Record.mk_closed
  and init_state _ = Env.vars_rec false |> MT.Record.mk_closed
  and end_state_cast (pos,_ as e) st loc =
    pos, Cast
      ( (pos, DelStateFields (loc,e))
      , MT.(Tuple.mk [ TVar.(Some "res" |> mk KInfer |> typ)
                     ; st ])
        |> MlGTy.mk)
  in
  { get_cast; set_cast; mk_state
  ; end_state_cast
  ; fun_pair = true
  ; init_state
  ; pack_toplevel = true
  ; cast_toplevel = false
  }

let simple_cfg : ps_config =
  let get_cast = fun _ -> false
  and set_cast = fun _ -> false
  and mk_state (sid:Ast.scoped_identifiers) =
    let open Ast in
    let f b ident l =
      let name = Ast.Ident.name ident in
      let tv = Utils.mk_tv name in
      let ty = if b
        then tv
            (* Utils.undef *)
        else tv in
      (name, (ty, false))::l
    in
    IdentSet.(fold (f false) sid.nl_unused []
              |> fold (f false) sid.nl_used
              |> fold (f true) sid.locals)
    |> List.rev |> MT.Record.mk_closed
  and init_state (ids:Ast.IdentSet.t) =
    Ast.IdentSet.fold
      (fun id acc -> (MlVar.show id.name, (Utils.undef, false))::acc)
      ids []
    |> MT.Record.mk_closed
  and end_state_cast (pos,_ as e) _st loc =
    (* pos, Cast ( *)
        (pos, DelStateFields (loc,e))
      (* , MT.(Tuple.mk [ TVar.(Some "res" |> mk KInfer |> typ) *)
                     (* ; st ]) *)
        (* |> MlGTy.mk) *)
  in
  { get_cast; set_cast; mk_state
  ; end_state_cast
  ; fun_pair = true
  ; init_state
  ; pack_toplevel = false
  ; cast_toplevel = false
  }

let cfg = simple_cfg


let r_tag = MT.Tag.define "R"
and v_tag = MT.Tag.define "V"
let r_tag_t ty = MT.(Tag.mk r_tag ty )
and v_tag_t ty = MT.(Tag.mk v_tag ty )
let r_tag_gt = r_tag_t MT.Ty.any |> MlGTy.mk
and v_tag_gt = v_tag_t MT.Ty.any |> MlGTy.mk

let res_tag = function R -> r_tag | V -> v_tag


module PSBuiltins = struct
  let builtins : (string, (MlVar.t * Sstt.Ty.t)) Hashtbl.t =
    (Hashtbl.create 16)

  let find_opt s = Hashtbl.find_opt builtins s |> Option.map fst
  let add ?(kind=MlMVar.Immut) vname body =
    let open Hashtbl in
    if mem builtins vname
    then failwith (vname ^ " already exists")
    else
      let v = Utils.mk_var_t ~kind vname in
      add builtins vname (v,body);
      v

  let all () = builtins |> Hashtbl.to_seq_values |> List.of_seq
end


let mlvar = function
    Simple v -> v
  | Source(Ast.{name; _ }) -> name

let subst e s = (* unsound in general but ok since it's only called for
                   administrative lambdas that have unique argument names *)
  let rec loop (pos, e) =
    (pos, loop_expr e)
  and loop_expr e =
    match e with
    | Var id ->
      begin match List.find
                    (fun (v, _) -> MlVar.compare (mlvar v) (mlvar id) = 0) s
        with
        | (_, e') -> snd e'
        | exception Not_found -> e
      end
    | Res (k, e) -> Res (k, loop e)
    | Proj (k, e) -> Proj(k, loop e)
    | IfV r -> IfV { r with cond = loop r.cond; body = loop r.body }
    | IfR r -> IfR { r with cond = loop r.cond; body = loop r.body }
    | Ite (e1, e2, e3) -> Ite(loop e1, loop e2, loop e3)
    | Tuple l -> Tuple (List.map loop l)
    | Pi (i, e) -> Pi(i, loop e)
    | EmptyRec | Const _ -> e
    | RecUpdate (e1, id, e2) -> RecUpdate(loop e1, id, loop e2)
    | Field(e, id) -> Field (loop e, id)
    | Lambda (id, k, e) -> Lambda(id, k, loop e)
    | App (e1, e2) -> App (loop e1, loop e2)
    | DelStateFields (l, e) -> DelStateFields(l, loop e)
    | Cast (e, gty) -> Cast (loop e, gty)
  in loop e

let is_simple e =
  let rec loop (_, e) = loop_expr e
  and loop_expr = function
    | Var _ | EmptyRec | Const _  |Lambda _ -> true
    | Res (_, e) | Proj(_, e) | Pi (_ , e) | Field (e, _)
    | DelStateFields (_, e) | Cast (e, _) -> loop e
    | IfV { cond; body; _ } -> loop cond && loop body
    | IfR { cond; body; _ } -> loop cond && loop body
    | Ite(e1, e2, e3) -> loop e1 && loop e2 && loop e3
    | RecUpdate(e1, _, e2) -> loop e1 && loop e2
    | Tuple l -> List.for_all loop l
    | App _ -> false
  in
  loop e

let rec find_field (_, e) (f:ident) =
  match e with
    RecUpdate (e1, id, e2) -> if MlVar.equal (mlvar id) (mlvar f) then e2
    else find_field e1 f
  | _ -> raise Not_found

let reduce e =
  let rec loop (pos, e) = pos, loop_expr e
  and loop_expr e =
    match e with
    | Const _ | Var _ | EmptyRec -> e
    | Res (k,e) -> Res (k, loop e)
    | Proj (k,(p, e)) -> (match loop_expr e with
        | Res (k', (_,e')) when k = k' -> e'
        | e -> Proj (k,(p, e)))
    | IfV r -> (
        let cond = loop r.cond in
        let body = loop r.body in
        match snd cond with
        | Tuple [_,Res (R, _);_] as r -> r
        | Tuple [_,Res (V,e) ;s] -> snd (subst body [r.v, e; r.s, s])
        | _ -> IfV{r with cond; body }
      )
    | IfR r -> IfR r (* TODO *)
    | Ite(e1, e2, e3) -> Ite(loop e1, loop e2, loop e3)
    | Tuple l -> Tuple (List.map loop l)
    | Pi (i, e) -> (match loop e with
        | _, Tuple l when List.length l > i && List.for_all is_simple l ->
          snd (List.nth l i)
        | e' -> Pi(i, e')
      )
    | RecUpdate (e1, id, e2) -> RecUpdate(loop e1, id, loop e2)
    | Field (e, id) ->
      let e = loop e in
      if is_simple e then
        (match find_field e id with
           e' -> snd e'
         | exception Not_found -> Field (e,id))
      else Field(e,id)
    | DelStateFields(l, e) -> DelStateFields(l, loop e) (* TODO simplify *)
    | Lambda (id, k, e) ->  Lambda(id, k, loop e)
    | App(e1, e2) ->
      let e1 = loop e1 in
      let e2 = loop e2 in
      (match snd e1 with
       | Lambda(id, k, e) when is_admin k -> snd (loop (subst e [id, e2]))
       | _ -> App (e1, e2))
    | Cast (e, gty) -> Cast (loop e, gty)
  in loop e

let mk_ident s = Simple (Some Utils.(internal s) |> MlVar.create)
let mk_ident_v =
  let _, sn = Utils.gen_cpt () in
  fun () -> mk_ident ("v" ^ sn ())
let mk_ident_s =
  let _, sn = Utils.gen_cpt () in
  fun () -> mk_ident ("s" ^ sn ())


let cast_if cond id pos expr =
  match id with
  | Source (Ast.{name;scope}) ->
    let _scope_name,_,ty = Env.get_var_infos name in
    if cond scope
    then Cast ((pos,expr), MlGTy.mk ty)
    else expr
  | _ -> expr

let pair pos e1 e2 =
  pos, Tuple[e1; e2]

let res pos k e =
  pos, Res(k, e)

let emon_ret pos k e s =
  pos, Tuple [ (pos, Res (k, e)) ; (pos, Var s) ]
and emon_upd_l pos k res s ie_l =
  let rec fold acc = function
    | [] -> acc
    | (id,e)::l -> fold (pos, RecUpdate (acc, id, (pos,e))) l
  in
  pos, Tuple [ (pos, Res (k, res)) ; fold (pos, Var s) ie_l ]
let emon_upd pos k res s id e =
  emon_upd_l pos k res s [id,e]
let emon_upd_v pos k res s id v = emon_upd pos k res s id (Var v)
let smon_ret pos s (sty,idl) e =
  pos, Lambda (s, State (sty,idl), e)
let smon_run pos e s =
  pos, App (e, (pos, Var s))

let bindV pos e s0 v s body =
  let cond = smon_run pos e s0 in
  pos, IfV { cond; v; s; body }
and bindR pos e s0 v s body =
  let cond = smon_run pos e s0 in
  pos, IfR { cond; v; s; body }

let app pos e1 e2 =
  pos, App(e1, e2)
let none = Ast.None_

(* Combinators *)
let const pos st c =
  let s = mk_ident_s () in
  smon_ret pos s st
    (emon_ret pos V (pos, Const c) s)

let var_get pos st id =
  let s = mk_ident_s () in
  let proj = Field ((pos, Var s), id) in
  emon_ret pos V (pos, cast_if cfg.get_cast id pos proj) s
  |> smon_ret pos s st

let var_set pos st id e =
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s () in
  let v = mk_ident_v () in
  smon_ret pos s0 st
    (bindV pos e s0
       v s1 (emon_upd pos V (pos, Const none) s1 id
               (cast_if cfg.set_cast id pos (Var v))))

let return pos st e =
  let s0 = mk_ident_s ()
  and s = mk_ident_s () in
  let v = mk_ident_v () in
  smon_ret pos s0 st
    (bindV pos e s0 v s
       (emon_ret pos R (pos, Var v) s))

let ite pos st cond e1 e2 =
  let s0 = mk_ident_s ()
  and s = mk_ident_s () in
  let v = mk_ident_v () in
  let ps = pos, Var s in
  smon_ret pos s0 st
    (bindV pos cond s0
       v s (pos, Ite
              ((pos, Var v),
               app pos e1 ps,
               app pos e2 ps)))

let seq pos st e1 e2 =
  let s0 = mk_ident_s ()
  and s = mk_ident_s () in
  let v = mk_ident_v () in
  smon_ret pos s0 st
    (bindV pos e1 s0
       v s (smon_run pos e2 s))

let lambda pos is_expr (sty,_ as st) x (_,locals as lam_st) e =
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s ()
  and s2 = mk_ident_s ()
  and s = mk_ident_s () in
  let v = mk_ident_v () in
  let xty = (* state-passing style or static tv? *)
    Utils.mk_tv MlVar.(mlvar x |> show)
    (* Env.(Vartbl.find variables (mlvar x) |> fun (_,_,t) -> t) *)
  in
  let f_body =
    bindV pos
      (smon_ret pos s1 lam_st
         (emon_upd_v pos V (pos, Const none) s1 x x)) s0
      (mk_ident_v ()) s2
      (bindV pos e s2 v s
         (if is_expr (* treat Python's lambdas as if there were a return *)
          then emon_ret pos R (pos, Var v) s
          else emon_ret pos V (pos, Const none) s)) in
  let f_body = cfg.end_state_cast f_body sty locals in
  let f =
    if cfg.fun_pair
    then let tp = MT.Tuple.mk [xty;sty] in
      pos, Lambda (mk_ident "p", Both (tp,x,xty,s0,lam_st),f_body)
    else
      pos, Lambda
        (x, Normal xty,
         smon_ret pos s0 st
           f_body) in
  let s0 = mk_ident_s () in
  smon_ret pos s0 st
    (emon_ret pos V f s0)

let apply pos st e1 e2 =
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s ()
  and s2 = mk_ident_s ()
  and s3 = mk_ident_s () in
  let f = mk_ident_v ()
  and x = mk_ident_v ()
  and v = mk_ident_v () in
  let bind_r =
    let end_ret = emon_ret pos V (pos, Var v) s3 in
    if cfg.fun_pair
    then pos, IfR
           { cond = app pos (pos, Var f) (pair pos (pos, Var x) (pos, Var s2));
             v; s = s3; body = end_ret}
    else bindR pos (app pos (pos, Var f) (pos, Var x)) s2
        v s3 end_ret in
  smon_ret pos s0 st
    (bindV pos e1 s0
       f s1 (bindV pos e2 s1
               x s2 bind_r))

let tuple2 pos st e1 e2 =
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s ()
  and s2 = mk_ident_s () in
  let v1 = mk_ident_v ()
  and v2 = mk_ident_v () in
  smon_ret pos s0 st
    (bindV pos e1 s0
       v1 s1 (bindV pos e2 s1
                v2 s2 (emon_ret pos V
                         (pos, Tuple [pos,Var v1; pos,Var v2]) s2)))

let binop pos st e1 (bop:Ast.binop) e2 =
  (* λs0. bind v1, s1 = [e1] s0 in
          bind v2, s2 = [e2] s1 in
          bind op, s3 = [op] s2 in
          ((op e1) e2) s3 (??) *)
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s ()
  and s2 = mk_ident_s () in
  let v1 = mk_ident_v ()
  and v2 = mk_ident_v () in
  let op =
    let bop_str, ty =
      let end_ty t = t
        (* let state = *)
        (*   (\* st *\) *)
        (*   MT.(Ty.conj [Utils.mk_tv "σ"; Record.any]) *)
        (* in *)
        (* MT.(Arrow.mk state (Tuple.mk [v_tag_t t; state])) *)
      in
      let pol_cmp _ =
        let tv = Utils.mk_tv "θ" in
        MT.( Arrow.mk tv      (Arrow.mk tv      (end_ty Ty.int))) in
      Ast.Binop.str_ty
        MT.( Arrow.mk Ty.int  (Arrow.mk Ty.int  (end_ty Ty.int))
           , Arrow.mk Ty.bool (Arrow.mk Ty.bool (end_ty Ty.bool))
           , Arrow.mk Ty.int  (Arrow.mk Ty.int  (end_ty Ty.bool))
           , pol_cmp) bop
    in
    let bop_str = Utils.mk_id "%s" bop_str in
    Simple (match PSBuiltins.find_opt bop_str with
        | Some v -> v
        | None -> PSBuiltins.add bop_str ty)
  in
  smon_ret pos s0 st
    (bindV pos e1 s0
       v1 s1 (bindV pos e2 s1
                v2 s2 (emon_ret pos V
                         (app pos
                            (app pos
                               (pos, Var op)
                               (pos, Var v1))
                            (pos, Var v2))
                         s2) ))

(* Translation pysem -> pystate *)

module HMlVar = Hashtbl.Make(struct
    include MlVar
    let hash v = String.hash (MlVar.show v)
  end)

let ty_var =
  let h = HMlVar.create 16 in
  fun mlv ->
    match HMlVar.find_opt h mlv with
      Some t -> t
    | None ->
      let t = Utils.mk_tv ~k:MT.KInfer ("ɑ" ^ MlVar.show mlv) in
      HMlVar.add h mlv t; t

let row_id = ref 0
let make_state_record _sid =
  failwith "should be unused" (*
  (* ; `si row variable but this is too expensive ! *)
  let field_row = MT.RVar.(
      mk KInfer (Some (Format.sprintf "s%d" !row_id)) |> fty) in
  let absent = MT.(FTy.of_oty (Ty.empty,true)) in
  let _tail = MT.RVar.(mk KInfer (Some (Format.sprintf "s%d" !row_id))|> fty) in
  let ids = Ast.(IdentSet.union sid.nl_used
                   (IdentSet.union sid.nl_unused sid.locals)) in
  incr row_id;
  MT.Record.mk' field_row
    (List.filter_map (fun id ->
         match id.Ast.scope with
         | Parsing.(Local true) -> None
         | _ ->
           let v = id.Ast.name in
           Some (MlVar.show v,
                 if Ast.IdentSet.mem id sid.locals then absent
                 else field_row
                ))
        (Ast.IdentSet.to_list ids)) *)

let make_unique_state_type () =
  Env.(Vartbl.fold
         (fun mlvar (_scope, _tv, typv) acc ->
            (MlVar.show mlvar, (typv, true))::acc)
         variables [])
  |> MT.Record.mk_closed

let of_opt p st eo f = match eo with
  | None -> const p st none
  | Some e -> f st e

let rec of_expr st (p,e:Ast.expr) : expr = match e with
  | Var id -> (* λs.V(s.x),s *)
    var_get p st (Source id)
  | Binop (e1,bop,e2) ->
    (* λs0. bind v1, s1 = [e1] s0 in
            bind v2, s2 = [e2] s1 in
            bind op, s3 = [op] s2 in
            ((op e1) e2) s3 (??) *)
    let e1 = of_expr st e1 in
    let e2 = of_expr st e2 in
    binop p st e1 bop e2
  | Cst c -> (* λs.V(c),s *)
    const p st c
  | Lambda (spec,sid,body) ->
    let lam_st = cfg.mk_state sid in
    let locals = sid.locals |> Ast.IdentSet.to_list
      |> List.map (fun id -> Source id)
    in
    let x = of_spec spec in
    let e = of_expr st body in
    lambda p true st x (lam_st,locals) e
  | Apply (e,param) ->
    (* λs0. bind f, s1 = [e] s0 in
            bind x, s2 = [p] s1 in
            (f x) s2 *)
    let e1 = of_expr st e
    and e2 = of_param st param in
    apply p st e1 e2
  | Tuple [e1;e2] ->
    let e1 = of_expr st e1
    and e2 = of_expr st e2 in
    tuple2 p st e1 e2
  | Tuple _ ->
    (* λs0. bind x1, s1 = [e1] s0 in ... bind xn, sn = [en] sn-1 in
            (x1, ..., xn), sn *)
    failwith "TODO all tuples"

and of_spec {posonly;mixed;_} = match posonly with
  | [ (x,None) ] -> Source x
  | _ ->  match mixed with
    | [ (x,None) ] -> Source x
    | _ -> failwith "TODO not just 1 posonly"

and of_param st {pos;_} = match pos with
  | [] -> Var (mk_ident "dummy") |> Ast.dannot
  | [ e ] -> of_expr st e
  | _ -> failwith "TODO not just 1 pos parameter"

let rec of_instr st (p,i:Ast.instr) = match i with
  | Block il ->
    begin match List.map (of_instr st) il with
      | [] -> const p st none
      | e1::l -> List.fold_left (fun sq ((p,_) as e) -> seq p st sq e ) e1 l
    end
  | FunDef (id, spec, sid, body)  ->
    (* Format.printf "Function %a has scope: used:%a,unused:%a\n%!"
      Ast.Ident.pp_full id Ast.IdentSet.pp sid.Ast.nl_used
      Ast.IdentSet.pp sid.Ast.nl_unused; *)
    let lam_st = cfg.mk_state sid in
    let locals = sid.locals |> Ast.IdentSet.to_list
      |> List.map (fun id -> Source id)
    in
    let x = of_spec spec in
    let e = of_instr st body in
    var_set p st (Source id)
      (lambda p false st x (lam_st,locals) e)
  | Return eo ->
    of_opt p st eo of_expr
    |> return p st
  | Assign (tg, e) ->
    var_set p st (Source tg) (of_expr st e)
  | While (_e, _i) -> failwith "TODO while control flow"
  | If (test, i, io) ->
    ite p st
      (of_expr st test)
      (of_instr st i)
      (of_opt p st io of_instr)
  | Iexpr e -> of_expr st e
  | Break | Continue -> failwith "TODO while control flow"

let of_prog (p,g:Ast.prog) =
  let g_id = Ast.IdentSet.to_list g in
  let tyrec = List.map (fun id ->
      let name = Ast.Ident.name id in
      (name, (Utils.mk_tv name, false))) g_id
    |> MT.Record.mk_closed in
  List.map (of_instr (tyrec, List.map (fun aid -> Source aid) g_id)) p

let ty_undef = MT.Enum.(define "%py_uninitialized" |> typ)
let initial_env ids =
  Utils.mk_rec_disj false
    [(List.map (fun id ->
         let v =  id.Ast.name in
         (MlVar.show v, (ty_undef, false)))
         (Ast.IdentSet.to_list ids))]

(* Translation pystate -> mlsem *)

let mk_ident_ml =
  let _, sn = Utils.gen_cpt () in
  fun () -> mk_ident ("m" ^ sn ())
let ident_name id = mlvar id |> MlVar.show

let cpt = Utils.gen_cpt () |> snd

let mk_match p tag_gt tag cond v s body =
  let open Utils in
  let c_ml = mk_ident_ml () |> mlvar in
  let r_ml = mk_ident_ml () |> mlvar in
  let c_var = var_of_vart p c_ml in
  let r_var = var_of_vart p r_ml in
  mk_let p []
    c_ml cond
    (mk_let p []
       r_ml (mk_proj_tuple p 2 0 c_var)
       (mk_ite p r_var tag_gt
          (mk_let p []
             (mlvar v) (mk_proj_tag p tag r_var)
             (mk_let p []
                (mlvar s) (mk_proj_tuple p 2 1 c_var)
                body))
          (c_var)))

let mk_delete_fields p e l =
  let open Utils in
  let x = mk_ident_ml () |> mlvar in
  let xt = x |> var_of_vart p in
  let v1 = mk_proj_tuple p 2 0 xt in
  let v2 = mk_proj_tuple p 2 1 xt in
  let dv2 = List.fold_left (fun e f ->
      mk_rec_del p (ident_name f) e
    ) v2 l
  in
  mk_let p [] x e
    (mk_tuple p [v1; dv2])

let rec to_ml (p,e) =
  let open Utils in
  match e with
  | Const c -> mk_value p Ast.Const.(to_gty c)
  | Var id -> mlvar id |> var_of_vart p
  | Res (r, r_e) -> mk_tag p (res_tag r) (to_ml r_e)
  | Proj (r, p_e) -> mk_proj_tag p (res_tag r) (to_ml p_e)
  | IfV {cond;v;s;body} ->
    mk_match p v_tag_gt v_tag (to_ml cond) v s (to_ml body)
  | IfR {cond;v;s;body} ->
    mk_match p r_tag_gt r_tag (to_ml cond) v s (to_ml body)
  | Ite (test, e1, e2) ->
    mk_ite p (to_ml test) MT.(GTy.mk Ty.tt) (to_ml e1) (to_ml e2)
  | Tuple el -> mk_tuple p List.(map to_ml el)
  | Pi (i,e) -> mk_proj_tuple p 2 i (to_ml e) (* FIXME arity 2 hard coded! *)
  | EmptyRec -> mk_record p [] []
  | RecUpdate (r, x, e) -> mk_record_update p (ident_name x) (to_ml e) (to_ml r)
  | Field (e, id) -> mk_projection p (MSAst.PiField (ident_name id)) (to_ml e)
  | Lambda (id,k,e) ->
    (* let tyvar = MT.TVar.(mk KInfer (Some ("α" ^ cpt ())) |> typ) in *)
    let ty,idl,bth = match k with
      | Normal t -> t, [], None
      | State (t,idl) -> t, idl, None (* MT.Ty.cap t tyvar *)
      | Both (tp,x,tx,s,(ts,idl)) -> tp, idl, Some (x,tx,s,ts)
    in
    let gty = MT.(ty |> GTy.mk) in
    let ml_id = mlvar id
    and ml_e = to_ml e in
    let id_var = var_of_vart p ml_id in
    let rec init_loc e = function
      | [] -> e
      | id::l -> init_loc (mk_record_update p
                             (mlvar id |> MlVar.show)
                             (MlGTy.mk undef |> mk_value p ) e) l in
    mk_lambda p [] gty
      ml_id
      (match bth with
       | None -> ml_e
       | Some (x,tx,s,ts) ->
         mk_let p [tx] (mlvar x) (mk_proj_tuple p 2 0 id_var)
           (mk_let p [ts] (mlvar s)
              (init_loc (mk_proj_tuple p 2 1 id_var) idl)
              ml_e))
  | App (e1, e2) -> mk_app p (to_ml e1) (to_ml e2)
  | DelStateFields (l, e) -> mk_delete_fields p (to_ml e) l
  | Cast (e, gty) -> mk_coerce p (to_ml e) gty MSAst.Check

let fold_ml _global_ids ml_l =
  let open Utils in
  let cpt = gen_cpt () |> snd in
  let mk_tmp_state () =
    "S" ^ cpt () |> internal |> mk_var_t ~kind:MlMVar.Immut in
  let rec loop el (s, acc) = match el with
    | [] -> acc
    | (p,e)::l ->
      let s2 = mk_tmp_state () in
      ( s2
      , ( s2
        , mk_app dummy_pos (p,e) (var_of_vart dummy_pos s)
          |> mk_proj_tuple dummy_pos 2 1) :: acc)
      |> loop l in
  let s = mk_tmp_state () in
  (* let _gty = initial_env _global_ids |> MT.GTy.mk in *)
  (s, [ s, mk_record dummy_pos [][] ])
  |> loop ml_l |> List.rev

let prepare_toplevel le =
  let open Utils in
  let var i = mk_var_t ~kind:MlMVar.Immut
      (internal Format.(sprintf "s%d" i))
  in
  let v0 = var 0 in
  let _, _, le =
    List.fold_left (fun (i, v, accl) (p, e) ->
        let vn = var (i+1) in
        let e1 : expr = (p,e) in
        let e2 : expr = (p, Var (Simple v)) in
        (i+1, vn, (vn, (p, (Pi (1, (p, App (e1,e2))))))
                  ::accl))
      (0, v0, [v0,(MC.Position.dummy, EmptyRec)]) le
  in
  List.rev le

(* Print *)

let show_res_kind = function R -> "R" | V -> "V"
let pp_res_kind fmt r = Format.fprintf fmt "%s" (show_res_kind r)

let pp_ident fmt = function
  | Simple mlv -> Printing.MLAstPrinter.pp_variable fmt mlv
  | Source id -> Ast.Ident.pp fmt id

let rec pp_expr'_recupd fmt r0 f0 v0 =
  let open Format in
  let rec get_recupd acc ti = match ti with
    | _, RecUpdate (tj, fi, vi) ->
      get_recupd ((fi,vi)::acc) tj
    | _ -> acc, ti in
  let pp_base fmt r = match r with
    | _, EmptyRec -> ()
    | _ -> fprintf fmt "%a @{<bold>with@}@ " pp_expr r
  and pp_var fmt (fi,vi) = fprintf fmt "%a =@ %a" pp_ident fi pp_expr vi in
  let fv_l, ti = get_recupd [f0,v0] r0 in
  fprintf fmt "@[<hov 2>{ %a%a }@]"
    pp_base ti (Printing.pp_list pp_var) fv_l

and pp_expr' fmt e =
  let open Format in
  match e with
  | Const c -> Ast.pp_const fmt c
  | Var id -> pp_ident fmt id
  | Res (r, e) -> fprintf fmt "@[<hov 2>%a(%a)@]" pp_res_kind r pp_expr e
  | Proj (r, e) -> fprintf fmt "@[<hov 2>(%a).%a@]" pp_expr e pp_res_kind r
  | IfV {cond;v;s;body} ->
    fprintf fmt "@[@[<hov 2>@{<bold>bindv@} %a, %a =@ %a@] \
                 @{<bold>in@}@ %a@]"
      pp_ident v pp_ident s pp_expr cond pp_expr body
  | IfR {cond; v; s; body} ->
    fprintf fmt "@[@[<hov 2>@{<bold>bindr@} %a, %a =@ %a@] \
                 @{<bold>in@}@ %a@]"
      pp_ident v pp_ident s pp_expr cond pp_expr body
  | Ite (e1, e2, e3) ->
    fprintf fmt
      "@[<v>@[<hov 2>@{<bold>if@} %a@]@ \
       @[<hov 2>@{<bold>then@} %a@]@ \
       @[<hov 2>@{<bold>else@} %a@]@]"
      pp_expr e1 pp_expr e2 pp_expr e3
  | Tuple el ->
    fprintf fmt "@[(@[%a@])@]" (Printing.pp_list ~sep:",@ " pp_expr) el
  | Pi (i, e) -> fprintf fmt "@[@{<bold>π%d@}(%a)@]" i pp_expr e
  | EmptyRec -> fprintf fmt "{}"
  | RecUpdate (e1, id, e2) -> pp_expr'_recupd fmt e1 id e2
  | Field (e, id) -> fprintf fmt "@[%a@,.%a@]" pp_expr e pp_ident id
  | DelStateFields(l, e) ->
    fprintf fmt "@[%a@,\\{%a}@]"
      pp_expr e Printing.(pp_list ~sep:",@ " pp_ident) l
  | Lambda (id, State (ty,_), e) ->
    fprintf fmt "@[<hov 2>ƛ %a @{<bold;purple>: %a@}@{<bold>.@}@ %a@]"
      pp_ident id MT.Ty.pp ty pp_expr e
  | Lambda (id, Normal ty, e) ->
    fprintf fmt "@[<hov 2>λ %a @{<bold;purple>: %a@}@{<bold>.@}@ %a@]"
      pp_ident id MT.Ty.pp ty pp_expr e
  | Lambda (id, Both (tp,x,_,s,_), e) ->
    fprintf fmt "@[<hov 2>λƛ (%a, %a) @{<bold>as@} %a \
                 @{<bold;purple>: %a@}@{<bold>.@}@ %a@]"
      pp_ident x pp_ident s pp_ident id
      MT.Ty.pp tp pp_expr e
  | App (e1, e2) -> fprintf fmt "@[<hov 2>(%a)@ %a@]" pp_expr e1 pp_expr e2
  | Cast (e, gty) ->
    fprintf fmt "@[<hov 2>@{<bold;purple>(@}%a@{<bold;purple>)@ :> @[%a@]@}@]"
      pp_expr e MlGTy.pp gty
and pp_expr fmt (_,e) = pp_expr' fmt e
