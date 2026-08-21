open Aliases

type res_kind = R | V
type ident = Simple of MlVar.t
           | Source of Ast.ident
type fun_ctx = {
  (* λp.e = λ(x,s).let s_arg = { s with ⋯ } in let ⋯ = e s_arg in ⋯ *)
  pty : Sstt.Ty.t ; (* type of p, the pair argument (x,s) *)
  xid : ident ; xty : Sstt.Ty.t ; (* id and type of x *)
  outer_sty : Sstt.Ty.t ; (* type of s (given state) *)
  nl_used : ident list ;
  inner_sid : ident ; inner_sty : Sstt.Ty.t ;
  (* id, and fields of s_arg (local state) *)
  locals : ident list ;
  add_ret : bool ; (* should we return the produced value? *)
}
type lambda_kind =
  | Normal of fun_ctx
  | State of Sstt.Ty.t (* record *) * (ident list) (* domain of the record *)
type call_kind = Normal_call | State_call
let is_admin = function State _ -> true | _ -> false
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
  | App of call_kind * expr * expr
  | Cast of expr * MlGTy.t
and expr = MC.Position.t * expr'

let r_tag = MT.Tag.define "R"
and v_tag = MT.Tag.define "V"
let r_tag_t ty = MT.(Tag.mk r_tag ty )
and v_tag_t ty = MT.(Tag.mk v_tag ty )
let r_tag_gt = r_tag_t MT.Ty.any |> MlGTy.mk
and v_tag_gt = v_tag_t MT.Ty.any |> MlGTy.mk

let mk_fun_cast p f =
  let vdom = Utils.mk_tv ~k:MT.KInfer "i" in
  let vimg = Utils.mk_tv ~k:MT.KInfer "o" in
  let v_in_st = Utils.mk_rtv ~k:MT.KInfer "si" in
  let v_out_st = Utils.mk_rtv ~k:MT.KInfer "so" in
  let open MT in
  let i = Tuple.mk [ vdom; Record.mk' v_in_st [] ] in
  let o = Tuple.mk [ r_tag_t vimg; Record.mk' v_out_st [] ] in
  let ty = GTy.mk (Arrow.mk i o) in
  Utils.mk_coerce p
    ~check:MSAst.CheckStatic
    f ty

let res_tag = function R -> r_tag | V -> v_tag

let source x = Source x

(* Config variables *)
let _, scpt = Utils.gen_cpt ()
let get_cast = Parsing.(function Local _ -> false | _ -> true)
and set_cast = fun _ -> false
and mk_lambda_states (sid:Ast.scoped_identifiers) =
  let open Ast in
  let mk_rec l = List.fold_left (fun acc ident ->
      let name = Ast.Ident.name ident in
      let _,_,tv = Env.get_var_infos ident.name in
      (name, MT.FTy.of_oty (tv, false))::acc) [] l
    |> List.rev
    |> MT.Record.mk' ("row" ^ scpt () |> Utils.mk_rtv)
  in
  let outer_sty = sid.nl_used |> IdentSet.to_list |> mk_rec in
  let inner_sty =
    IdentSet.(union sid.nl_used sid.locals |> to_list)
    |> mk_rec
  in
  inner_sty, outer_sty
and init_state (ids:Ast.IdentSet.t) =
  Ast.IdentSet.fold
    (fun id acc -> (MlVar.show id.name, (Utils.undef, false))::acc)
    ids []
  |> MT.Record.mk_closed
and pack_toplevel = false
and cast_toplevel = false

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
let ident_name id = mlvar id |> MlVar.show

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
    | App (c, e1, e2) -> App (c,  loop e1, loop e2)
    | DelStateFields (l, e) -> DelStateFields(l, loop e)
    | Cast (e, gty) -> Cast (loop e, gty)
  in loop e

let is_simple e =
  let rec loop (_, e) = loop_expr e
  and loop_expr = function
    | Var _ | EmptyRec | Const _ | Lambda _ -> true
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
    | IfR r -> (
        let cond = loop r.cond in
        let body = loop r.body in
        match snd cond with
        | Tuple [_,Res (V, _);_] as r -> r
        | Tuple [_,Res (R,e) ;s] -> snd (subst body [r.v, e; r.s, s])
        | _ -> IfR{r with cond; body }
      )
    (* IfR r *)
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
    | DelStateFields(l, e) ->
      let e = loop e in
      (match e with
       | _, RecUpdate ((p,e1), id, _) when List.mem id l ->
         DelStateFields(l, loop (p,e1))
       | _ -> DelStateFields(l, e))
    | Lambda (id, k, e) -> Lambda (id, k, loop e)
    | App(c, e1, e2) ->
      let e1 = loop e1 in
      let e2 = loop e2 in
      (match snd e1 with
       | Lambda(id, k, e) when is_admin k -> snd (loop (subst e [id, e2]))
       | _ -> App (c, e1, e2))
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
  | Source (Ast.{scope;_}) ->
    if cond scope
    then Cast ((pos,expr), MT.Ty.neg Utils.undef |> MlGTy.mk)
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
  pos, App (State_call, e, (pos, Var s))

let bindV pos e s0 v s body =
  (* bindv v, s = e s0 in body *)
  let cond = smon_run pos e s0 in
  pos, IfV { cond; v; s; body }
and bindR pos e s0 v s body =
  (* bindr v, s = e s0 in body *)
  let cond = smon_run pos e s0 in
  pos, IfR { cond; v; s; body }

let app pos e1 e2 =
  pos, App(Normal_call, e1, e2)

let none = Ast.None_

(* Combinators *)
let const pos st c =
  (* λ s. V(c), s *)
  let s = mk_ident_s () in
  smon_ret pos s st
    (emon_ret pos V (pos, Const c) s)

let var_get pos st id =
  (* λ s. V(s.id :> ~Undef), s *)
  let s = mk_ident_s () in
  let proj = Field ((pos, Var s), id) in
  smon_ret pos s st
    (emon_ret pos V (pos, cast_if get_cast id pos proj) s)

let var_set pos st id e =
  (* λ s0. bindv v, s1 = e s0 in V(None), { s1 with id = v } *)
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s () in
  let v = mk_ident_v () in
  smon_ret pos s0 st
    (bindV pos e s0
       v s1 (emon_upd pos V (pos, Const none) s1 id
               (cast_if set_cast id pos (Var v))))

let return pos st e =
  (* λ s0. bindv v, s1 = e s0 in R(v), s1 *)
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s () in
  let v = mk_ident_v () in
  smon_ret pos s0 st
    (bindV pos e s0 v s1
       (emon_ret pos R (pos, Var v) s1))

let ite pos st cond e1 e2 =
  (* λ s0. bindv v, s1 = cond s0 in
           if v in True
           then e1 s1
           else e2 s1 *)
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s () in
  let v = mk_ident_v () in
  let ps1 = pos, Var s1 in
  smon_ret pos s0 st
    (bindV pos cond s0
       v s1 (pos, Ite
              ((pos, Var v),
               app pos e1 ps1,
               app pos e2 ps1)))

let seq pos st e1 e2 =
  (* λ s0. bindv _v, s = e1 s0 in e2 s1 *)
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s () in
  let v = mk_ident_v () in
  smon_ret pos s0 st
    (bindV pos e1 s0
       v s1 (smon_run pos e2 s1))

let mk_fun_ctx xid sid add_ret =
  let inner_sty, outer_sty = mk_lambda_states sid in
  let locals = sid.locals |> Ast.IdentSet.to_list |> List.map source in
  let nl_used = sid.nl_used |> Ast.IdentSet.to_list |> List.map source in  
  let _,_,xty = Env.get_var_infos (mlvar xid) in
  { pty = MT.Tuple.mk [xty;outer_sty];
    xid; xty;
    outer_sty; nl_used ;
    inner_sid = mk_ident_s (); inner_sty; locals;
    add_ret }

let lambda pos fun_ctx e_body =
  (* λ s. V(λ p.fun_ctx. e), s *)
  let f = pos, Lambda (mk_ident "p", Normal fun_ctx,e_body) in
  let s = mk_ident_s () in
  smon_ret pos s (MT.Record.mk' Utils.("row" ^ scpt () |>  mk_rtv)  [],[])
    (emon_ret pos V f s)

let apply pos st e1 e2 =
  (* λ s0. bindv f, s1 = e1 s0 in
           bindv x, s2 = e2 s1 in
           bindr r, s3 = f (x,s2) in
           V(r), s3 *)
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s ()
  and s2 = mk_ident_s ()
  and s3 = mk_ident_s () in
  let f = mk_ident_v ()
  and x = mk_ident_v ()
  and r = mk_ident_v () in
  let app =
    pos,
    IfR { cond = app pos (pos, Var f) (pair pos (pos, Var x) (pos, Var s2));
          v = r; s = s3; body = emon_ret pos V (pos, Var r) s3}
  in
  smon_ret pos s0 st
    (bindV pos e1 s0
       f s1 (bindV pos e2 s1
               x s2 app))

let tuple2 pos st e1 e2 =
  (* λ s0. bindv v1, s1 = e1 s0 in
           bindv v2, s2 = e2 s1 in
           V((v1,v2)), s2 *)
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
  (* λs0. bindv v1, s1 = e1 s0 in
          bindv v2, s2 = e2 s1 in
          (* op in builtins *)
          bindr v3, s3 = op ((v1,v2),s2) in
          V(v3), s3 *)
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s ()
  and s2 = mk_ident_s ()
  and s3 = mk_ident_s () in
  let v1 = mk_ident_v ()
  and v2 = mk_ident_v ()
  and v3 = mk_ident_v () in
  let op = (* get unique operator id (create and register if doesn't exist) *)
    let bop_str, ty =
      let s_tv = MT.Record.mk' (Utils.mk_rtv ~k:MT.KInfer "ρ") [] in
      let bop_arrow a b o =
        let open MT in
        Arrow.mk Tuple.(mk [mk [a;b]; s_tv]) (Tuple.mk [r_tag_t o; s_tv])
      in
      let pol_cmp _ =
        let tv = Utils.mk_tv "θ" in
        bop_arrow tv tv MT.Ty.bool
      in
      Ast.Binop.str_ty
        MT.( bop_arrow Ty.int  Ty.int  Ty.int
           , bop_arrow Ty.bool Ty.bool Ty.bool
           , bop_arrow Ty.int  Ty.int  Ty.bool
           , pol_cmp) bop
    in
    let bop_str = Utils.mk_id "%s" bop_str in
    Simple (match PSBuiltins.find_opt bop_str with
        | Some v -> v
        | None -> PSBuiltins.add bop_str ty)
  in
  let app =
    pos,
    IfR { cond = app pos
              (pos, Var op)
              (pair pos (pos, Tuple [pos, Var v1; pos, Var v2]) (pos, Var s2));
          v = v3; s = s3; body = emon_ret pos V (pos, Var v3) s3}
  in
  smon_ret pos s0 st
    (bindV pos e1 s0
       v1 s1 (bindV pos e2 s1
          v2 s2 app))

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
    let x = of_spec spec in
    let fun_ctx = mk_fun_ctx x sid true in
    let e = of_expr (fun_ctx.inner_sty, fun_ctx.locals@fun_ctx.nl_used) body in
    lambda p fun_ctx e
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
  | _ -> match mixed with
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
  | FunDef (id, spec, sid, body) ->
    let x = of_spec spec in
    let fun_ctx = mk_fun_ctx x sid false in
    let e = of_instr (fun_ctx.inner_sty, fun_ctx.locals@fun_ctx.nl_used) body in
    var_set p st (Source id)
      (lambda p fun_ctx e)
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
  let empty = Ast.IdentSet.empty in
  let inner,_ = mk_lambda_states {nl_used=empty; nl_unused=empty; locals=g} in
  List.(map (of_instr (inner, map source Ast.IdentSet.(to_list g)))) p

let ty_undef = MT.Enum.(define "%py_uninitialized" |> typ)
let initial_env ids =
  Utils.mk_rec_disj false
    [(List.map (fun id ->
         let v = id.Ast.name in
         (MlVar.show v, (ty_undef, false)))
         (Ast.IdentSet.to_list ids))]

(* Translation pystate -> mlsem *)

let mk_ident_ml =
  let _, sn = Utils.gen_cpt () in
  fun () -> mk_ident ("m" ^ sn ())

let cpt = Utils.gen_cpt () |> snd

let mk_match p tagpos tagneg cond v s body =
  let open Utils in
  let tagp_gt = MT.(Tag.mk tagpos Ty.any) |> MlGTy.mk in
  let tagn_gt = MT.(Tag.mk tagneg Ty.any) |> MlGTy.mk in
  let c_ml = mk_ident_ml () |> mlvar in (* result of cond: c_ml = (t(v),s) *)
  let r_ml = mk_ident_ml () |> mlvar in (* tag of result: t(v) *)
  let c_var = var_of_vart p c_ml in
  let r_var = var_of_vart p r_ml in
  (* let c = cond in
     let r = fst c in
     tif r is tag
     then let v = r#tag in
          let s = snd c in
          body
     else c *)
  mk_let p []
    c_ml cond
    (mk_let p []
       (mlvar s) (mk_proj_tuple p 2 1 c_var)
       (mk_let p []
          r_ml (mk_proj_tuple p 2 0 c_var)
          (mk_ite_approx p r_var tagp_gt
             (mk_let p []
                (mlvar v) (mk_proj_tag p tagpos
                             (mk_cast p ~check:MSAst.NoCheck
                                r_var
                                tagp_gt))
                body)
             (mk_tuple p [ (mk_cast p ~check:MSAst.NoCheck
                              r_var
                              tagn_gt)
                         ; mlvar s |> var_of_vart p]) )))

let mk_delete_fields p e l =
  List.fold_left (fun e f ->
      Utils.mk_rec_del p (ident_name f) e
    ) e l

let rec to_ml (p,e) =
  let open Utils in
  match e with
  | Const c -> mk_value p Ast.Const.(to_gty c)
  | Var id -> mlvar id |> var_of_vart p
  | Res (r, r_e) -> mk_tag p (res_tag r) (to_ml r_e)
  | Proj (r, p_e) -> mk_proj_tag p (res_tag r) (to_ml p_e)
  | IfV {cond;v;s;body} ->
    mk_match p v_tag r_tag (to_ml cond) v s (to_ml body)
  | IfR {cond;v;s;body} ->
    mk_match p r_tag v_tag (to_ml cond) v s (to_ml body)
  | Ite (test, e1, e2) ->
    mk_ite p (to_ml test) MT.(GTy.mk Ty.tt) (to_ml e1) (to_ml e2)
  | Tuple el -> mk_tuple p List.(map to_ml el)
  | Pi (i,e) -> mk_proj_tuple p 2 i (to_ml e) (* FIXME arity 2 hard coded! *)
  | EmptyRec -> mk_record p [] []
  | RecUpdate (r, x, e) -> mk_record_update p (ident_name x) (to_ml e) (to_ml r)
  | Field (e, id) -> mk_projection p (MSAst.PiField (ident_name id)) (to_ml e)
  | Lambda (id,k,e) ->
    let ml_e = to_ml e in
    let ml_id = mlvar id in
    let id_ty, body = match k with
      | Normal ctx ->
        let pid = var_of_vart p ml_id in
        let s_arg = mlvar ctx.inner_sid in
        let m = mk_ident_ml () |> mlvar in
        let mvar = var_of_vart p m in
        let r = mk_ident "tag" |> mlvar in
        let rvar = var_of_vart p r in
        (* initialize the local scope of the function
           - start from an *empty record*
           - copy the outer scope that is used
           - copy the parameter
           - initialize locals
             The parameter is in locals *)
        let input_state = mk_proj_tuple p 2 1 pid in
        let outer_f = ctx.nl_used |>
          List.map (fun id ->
              ident_name id,
              mk_projection p (MSAst.PiField (ident_name id)) input_state)
        in
        let param_f = [ (ident_name ctx.xid, mk_proj_tuple p 2 0 pid)] in
        let local_f = ctx.locals |>
          List.map (fun id ->
              let nid = ident_name id in
              (nid, if nid = ident_name ctx.xid then mk_proj_tuple p 2 0 pid
               else MlGTy.mk undef |> mk_value p))
        in
        let local_env =
          outer_f @ param_f @ local_f
          |> List.fold_left (fun acc (id, e) ->
              mk_record_update p id e acc
            ) (Utils.mk_record p [] [])
        in
        let restore_env s = List.fold_left
            (fun acc id -> mk_record_update p (ident_name id)
                (mk_projection p (MSAst.PiField (ident_name id)) s) acc )
            input_state ctx.nl_used in
        (* let s_arg = {p.input_state w/ x=p.x ; y=Undef ; ... }) in
           let m = e s_arg in
           let r = m.0 in
           ( R(r#V)
             |or|
             if r is V
             then R(None)
             else r
           , { p.input_state with g = (m.1) for g in dom(s_run) } )
        *)
        ctx.pty ,
        mk_let p []
          s_arg local_env
          (mk_let p []
             m (mk_app p ml_e (var_of_vart p s_arg))
             (mk_let p []
                r (mk_proj_tuple p 2 0 (var_of_vart p m))
                (mk_tuple p
                   [ if ctx.add_ret
                     then mk_tag p r_tag (mk_proj_tag p v_tag rvar)
                     else mk_ite p rvar v_tag_gt
                         (mk_tag p r_tag (to_ml (p, Const none)))
                         rvar
                   ; (*mk_cast p*) (mk_proj_tuple p 2 1 mvar |> restore_env)
                     (*(MlGTy.mk ctx.outer_sty)*)
                   ])))
      | State (sty,_idl) -> sty, ml_e
    in
    let ml_fun =
      mk_lambda p [] ~gty:MT.(GTy.mk id_ty)
        ml_id body
    in begin
      match k with
        State _ -> ml_fun
      | Normal _ -> mk_fun_cast p ml_fun
    end
  | App (c, e1, e2) ->
    let f = to_ml e1 in
    let f = match c with Normal_call -> mk_fun_cast p f | State_call -> f in
    mk_app p f (to_ml e2)
  | DelStateFields (l, e) -> mk_delete_fields p (to_ml e) l
  | Cast (e, gty) -> mk_cast p (to_ml e) gty

let _fold_ml _global_ids ml_l =
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
        (i+1, vn, (vn, (p, (Pi (1, (p, App (Normal_call, e1,e2))))))
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
  | Pi (i, e) -> fprintf fmt "@[<hov 2>@{<bold>π%d@}(%a)@]" i pp_expr e
  | EmptyRec -> fprintf fmt "{}"
  | RecUpdate (e1, id, e2) -> pp_expr'_recupd fmt e1 id e2
  | Field (e, id) -> fprintf fmt "@[%a@,.%a@]" pp_expr e pp_ident id
  | DelStateFields(l, e) ->
    fprintf fmt "@[<hov 2>{ %a@ without %a }@]"
      pp_expr e Printing.(pp_list ~sep:",@ " pp_ident) l
  | Lambda (id, State (ty,_), e) ->
    fprintf fmt "@[<hov 2>ƛ %a @{<bold;purple>: %a@}@{<bold>.@}@ %a@]"
      pp_ident id MT.Ty.pp ty pp_expr e
  | Lambda (id, Normal {pty;xid;xty;outer_sty;_}, e) ->
    fprintf fmt "@[<hov 2>λƛ (%a, _) @{<bold>as@} %a \
                 @{<bold;purple>: %a = %a, %a @}@{<bold>.@}@ %a@]"
      pp_ident xid pp_ident id
      MT.Ty.pp pty MT.Ty.pp xty MT.Ty.pp outer_sty pp_expr e
  | App (c, e1, e2) -> fprintf fmt "@[<hov 2>%s(%a)@ %a@]" 
                         (if c = Normal_call then "" else "!")
                         pp_expr e1 pp_expr e2
  | Cast (e, gty) ->
    fprintf fmt "@[<hov 2>@{<bold;purple>(@}%a@{<bold;purple>)@ :> @[%a@]@}@]"
      pp_expr e MlGTy.pp gty
and pp_expr fmt (_,e) = pp_expr' fmt e
