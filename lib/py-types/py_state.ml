open Aliases

type res_kind = R | V
type lambda_kind = Normal | State of Sstt.Ty.t
let is_admin = function Normal -> false | State _ -> true
type ident = Simple of MlVar.t
           | Source of Ast.ident
type expr' =
  | Const of Ast.const
  | Var of ident
  | Res of res_kind * expr
  | Proj of res_kind * expr
  | IfNotRes of {cond:expr; v:ident; s:ident; body:expr}
  | Val of {cond:expr; v:ident; s:ident; body:expr}
  | Ite of expr * expr * expr
  | Tuple of expr list
  | Pi of int * expr
  | EmptyRec
  | RecUpdate of expr * ident * expr
  | Field of expr * ident
  | DelStateFields of ident list * expr
  | Lambda of ident * lambda_kind * expr (* true ⇒ synthetic *)
  | App of expr * expr
and expr = MC.Position.t * expr'

let mlvar = function
    Simple v -> v
  | Source(Ast.{name; _ }) -> name

let subst e s = (* unsound in general but ok since it's only called for administrative lambdas
                   that have unique argument names *)
  let rec loop (pos, e) =
    (pos, loop_expr e)
  and loop_expr e =
    match e with
    | Var id ->
      begin match List.find (fun (v, _) -> MlVar.compare (mlvar v) (mlvar id) = 0) s with
          (_, e') -> snd e'
        | exception Not_found -> e
      end
    | Res (k, e) -> Res (k, loop e)
    | Proj (k, e) -> Proj(k, loop e)
    | IfNotRes r -> IfNotRes { r with cond = loop r.cond; body = loop r.body }
    | Val r -> Val { r with cond = loop r.cond; body = loop r.body }
    | Ite (e1, e2, e3) -> Ite(loop e1, loop e2, loop e3)
    | Tuple l -> Tuple (List.map loop l)
    | Pi (i, e) -> Pi(i, loop e)
    | EmptyRec | Const _ -> e
    | RecUpdate (e1, id, e2) -> RecUpdate(loop e1, id, loop e2)
    | Field(e, id) -> Field (loop e, id)
    | Lambda (id, k, e) -> Lambda(id, k, loop e)
    | App (e1, e2) -> App (loop e1, loop e2)
    | DelStateFields (l, e) -> DelStateFields(l, loop e)
  in loop e

let is_simple e =
  let rec loop (_, e) = loop_expr e
  and loop_expr = function
      Var _ | EmptyRec | Const _  |Lambda _ -> true
    | Res (_, e) | Proj(_, e) | Pi (_ , e) | Field (e, _) -> loop e
    | IfNotRes { cond; body; _ } -> loop cond && loop body
    | Val { cond; body; _ } -> loop cond && loop body
    | Ite(e1, e2, e3) -> loop e1 && loop e2 && loop e3
    | RecUpdate(e1, _, e2) -> loop e1 && loop e2
    | Tuple l -> List.for_all loop l
    | DelStateFields (_, e) -> loop e
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
    | IfNotRes r -> (
        let cond = loop r.cond in
        let body = loop r.body in
        match snd cond with
        | Tuple [_,Res (R, _);_] as r -> r
        | Tuple [_,Res (V,e) ;s] -> snd (subst body [r.v, e; r.s, s])
        | _ -> IfNotRes{r with cond; body }
      )
    | Val r -> Val r (* TODO *)
    | Ite(e1, e2, e3) -> Ite(loop e1, loop e2, loop e3)
    | Tuple l -> Tuple (List.map loop l)
    | Pi (i, e) -> (match loop e with
          _, Tuple l when List.length l > i && List.for_all is_simple l -> snd (List.nth l i)
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
      match snd e1 with
      | Lambda(id, k, e) when is_admin k -> snd (loop (subst e [id, e2]))
      | _ -> App (e1, e2)
  in loop e

let mk_ident s = Simple (Some Utils.(internal s) |> MlVar.create)
let mk_ident_v =
  let _, sn = Utils.gen_cpt () in
  fun () -> mk_ident ("v" ^ sn ())
let mk_ident_s =
  let _, sn = Utils.gen_cpt () in
  fun () -> mk_ident ("s" ^ sn ())
let pair pos e1 e2 =
  pos, Tuple[e1; e2]

let res pos k e =
  pos, Res(k, e)

let app pos e1 e2 =
  pos, App(e1, e2)
let none = Ast.None_

(* Combinators *)
let const pos st c =
  let s = mk_ident_s () in
  pos, Lambda
    (s, State st, pair pos
       (res pos V (pos, Const c))
       (pos, Var s) )

let var_get pos st id =
  let s = mk_ident_s () in
  pos, Lambda
    (s, State st, pair pos
       (res pos V (pos, Field ((pos, Var s), id)))
       (pos, Var s))

let var_set pos st id e =
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s () in
  let v = mk_ident_v () in
  pos, Lambda
    (s0, State st ,
     (pos, IfNotRes
        { cond=app pos e (pos, Var s0);
          v; s=s1;
          body=pair pos
              (res pos V (pos, Const none))
              (pos, RecUpdate ((pos, Var s1),id, (pos, Var v)))
        }))

let return pos st e =
  let s0 = mk_ident_s ()
  and s = mk_ident_s () in
  let v = mk_ident_v () in
  pos, Lambda
    (s0, State st,
     (pos,
      IfNotRes
        { cond = app pos e (pos, Var s0);
          v; s;
          body=
            pair pos
              (res pos R (pos, Var v))
              (pos, Var s)
        }))

let ite pos st cond e1 e2 =
  let s0 = mk_ident_s ()
  and s = mk_ident_s () in
  let v = mk_ident_v () in
  let ps = pos, Var s in
  pos, Lambda
    (s0, State st,
     (pos, IfNotRes
        { cond = app pos cond (pos, Var s0);
          v; s;
          body =
            pos, Ite
              ((pos, Var v),
               app pos e1 ps,
               app pos e2 ps)
        }))

let seq pos st e1 e2 =
  let s0 = mk_ident_s ()
  and s = mk_ident_s () in
  let v = mk_ident_v () in
  pos, Lambda
    (s0, State st,
     (pos, IfNotRes
        { cond = app pos e1 (pos, Var s0);
          v; s;
          body = app pos e2 (pos, Var s);
        }))

let lambda pos st x lam_st locals e =
  let s0  = mk_ident_s ()
  and s = mk_ident_v () in
  let v = mk_ident_v () in
  let f =
    Lambda
      (x, Normal,
       (pos, Lambda
          (s0, State lam_st,
           (pos, IfNotRes
              { cond = pos,
                       (DelStateFields(locals,app pos e
                                         (pos, RecUpdate ((pos, Var s0), x, (pos, Var x)))));
                v; s;
                body = pair pos (res pos V (pos, Const none)) (pos, Var s);
              }))))
  in
  let s0 = mk_ident_s () in
  pos, Lambda(s0, State st, pair pos (res pos V (pos,f)) (pos, Var s0))

let apply pos st e1 e2 =
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s ()
  and s2 = mk_ident_s ()
  and s3 = mk_ident_s () in
  let f = mk_ident_v ()
  and arg = mk_ident_v ()
  and v = mk_ident_v () in
  pos, Lambda
    (s0, State st,
     (pos, IfNotRes
        { cond = app pos e1 (pos, Var s0);
          v = f; s = s1;
          body =
            pos, IfNotRes
              { cond = app pos e2 (pos, Var s1);
                v = arg;
                s = s2;
                body =
                  pos, Val
                    { cond = app pos
                          (app pos (pos, Var f) (pos, Var arg))
                          (pos, Var s2);
                      v; s = s3;
                      body = pair pos
                          (res pos V (pos, Var v))
                          (pos, Var s3)
                    }
              }
        }))

let tuple2 pos st e1 e2 =
  let s0 = mk_ident_s ()
  and s1 = mk_ident_s ()
  and s2 = mk_ident_s () in
  let v1 = mk_ident_v ()
  and v2 = mk_ident_v () in
  pos, Lambda (s0, State st, (pos, IfNotRes {
      cond = app pos e1 (pos, Var s0);
      v = v1; s = s1;
      body = pos, IfNotRes {
          cond = app pos e2 (pos, Var s1);
          v = v2;
          s = s2;
          body = pair pos
              (res pos V (pair pos (pos, Var v1) (pos, Var v2)))
              (pos, Var s2)
        }}))

(* Translation pysem -> pystate *)

module HMlVar = Hashtbl.Make(struct
    include MlVar
    let hash v = String.hash (MlVar.get_unique_name v)
  end)

let ty_var =
  let h = HMlVar.create 16 in
  fun mlv ->
    match HMlVar.find_opt h mlv with
      Some t -> t
    | None ->
      let t = Utils.mk_tv ~k:MT.KInfer ("ɑ" ^ MlVar.get_unique_name mlv) in
      HMlVar.add h mlv t; t

let row_id = ref 0
let make_state_record sid =
  (* ; `si row variable but this is too expensive ! *)
  let field_row = MT.RVar.(mk KInfer (Some (Format.sprintf "s%d" !row_id))|> fty) in
  let absent = MT.(FTy.of_oty (Ty.empty,true)) in
  let _tail = MT.RVar.(mk KInfer (Some (Format.sprintf "s%d" !row_id))|> fty) in
  let ids = Ast.(IdentSet.union sid.nl_used (IdentSet.union sid.nl_unused sid.locals)) in
  incr row_id;
  MT.Record.mk' field_row
    (List.filter_map (fun id ->
         if id.Ast.scope = Parsing.Parameter then None
         else
           let v =  id.Ast.name in
           Some (MlVar.get_unique_name v,
                 if Ast.IdentSet.mem id sid.locals then absent
                 else field_row
                ))
        (Ast.IdentSet.to_list ids))

let of_opt p st eo f = match eo with
  | None -> const p st none
  | Some e -> f st e

let rec of_expr st (p,e:Ast.expr) : expr = match e with
  | Var id -> (* λs.V(s.x),s *)
    var_get p st (Source id)
  | Binop _ ->
    (* λs0. bind v1, s1 = [e1] s0 in
            bind v2, s2 = [e2] s1 in
            bind op, s3 = [op] s2 in
            ((op e1) e2) s3 (??) *)
    failwith "TODO Binop"
  | Cst c -> (* λs.V(c),s *)
    const p st c
  | Lambda (spec,sid,body) ->
    let lam_st = make_state_record sid in
    let locals = sid.locals |> Ast.IdentSet.to_list
                 |> List.map (fun id -> Source id)
    in
    let x = of_spec spec in
    let e = of_expr lam_st body in
    lambda p st x lam_st locals e
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

and of_spec {posonly;_} = match posonly with
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
    Format.printf "Function %a has scope: used:%a,unused:%a\n%!"
      Ast.Ident.pp_full id Ast.IdentSet.pp sid.Ast.nl_used Ast.IdentSet.pp sid.Ast.nl_unused;
    let lam_st = make_state_record sid in
    let locals = sid.locals |> Ast.IdentSet.to_list
                 |> List.map (fun id -> Source id)
    in
    let x = of_spec spec in
    let e = of_instr lam_st body in
    var_set p st (Source id) (lambda p st x lam_st locals e)
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

let of_prog p ids =
  let tyrec = make_state_record ids in
  List.map (of_instr tyrec) p

let ty_undef = MT.Enum.(define "%py_uninitialized" |> typ)
let initial_env ids =
  Utils.mk_rec_disj false
    [(List.map (fun id ->
         let v =  id.Ast.name in
         (MlVar.get_unique_name v, (ty_undef, false)))
         (Ast.IdentSet.to_list ids))]

(* Translation pystate -> mlsem *)

let mk_ident_ml =
  let _, sn = Utils.gen_cpt () in
  fun () -> mk_ident ("m" ^ sn ())
let ident_name id = mlvar id |> MlVar.get_unique_name

let r_tag = MT.Tag.define "R"
and v_tag = MT.Tag.define "V"
let r_tag_t = MT.(Tag.mk r_tag Ty.any )
and v_tag_t = MT.(Tag.mk v_tag Ty.any )
let r_tag_gt = MlGTy.mk r_tag_t
and v_tag_gt = MlGTy.mk v_tag_t

let res_tag =
  function R -> r_tag | V -> v_tag

let lcpt = Utils.gen_cpt () |> snd

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
  | IfNotRes {cond;v;s;body} -> mk_match p v_tag_gt v_tag cond v s body
  | Val {cond;v;s;body} -> mk_match p r_tag_gt r_tag cond v s body
  | Ite (test, e1, e2) ->
    mk_ite p (to_ml test) MT.(GTy.mk Ty.tt) (to_ml e1) (to_ml e2)
  | Tuple el -> mk_tuple p List.(map to_ml el)
  | Pi (i,e) -> mk_proj_tuple p 2 i (to_ml e) (* FIXME arity 2 hard coded! *)
  | EmptyRec -> mk_record p [] []
  | RecUpdate (r, x, e) -> mk_record_update p (ident_name x) (to_ml e) (to_ml r)
  | Field (e, id) -> mk_projection p (MSAst.PiField (ident_name id)) (to_ml e)
  | Lambda (id,k,e) ->
    let tyvar = MT.TVar.(mk KInfer (Some ("α" ^ lcpt ())) |> typ) in
    let ty = match k with
        Normal -> tyvar
      | State t -> t (*MT.Ty.cap t tyvar*)
    in
    let gty = MT.( ty |> GTy.mk) in
    mk_lambda p [] gty
      (mlvar id) (to_ml e)
  | App (e1, e2) -> mk_app p (to_ml e1) (to_ml e2)
  | DelStateFields (l, e) -> mk_delete_fields p (to_ml e) l

and mk_match p tag_gt tag cond v s body =
  let open Utils in
  let c = mk_ident_ml () in
  let r = mk_ident_ml () in
  let c_ml = mlvar c in
  let r_ml = mlvar r in
  let c_var = var_of_vart p c_ml in
  let r_var = var_of_vart p r_ml in
  mk_let p []
    c_ml (to_ml cond)
    (mk_let p []
       r_ml (mk_proj_tuple p 2 0 c_var)
       (mk_ite p r_var tag_gt
          (mk_let p []
             (mlvar v) (mk_proj_tag p tag r_var)
             (mk_let p []
                (mlvar s) (mk_proj_tuple p 2 1 c_var)
                (to_ml body)))
          (c_var)))

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
  (*let _gty = initial_env global_ids |> MT.GTy.mk in*)
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
        (i+1, vn, ((vn, (p, (Pi(1,(p, App(e1,e2))))))::accl)))
      (0, v0, [v0,(MC.Position.dummy, EmptyRec)]) le
  in
  List.rev le

(* Print *)

let show_res_kind = function R -> "R" | V -> "V"
let pp_res_kind fmt r = Format.fprintf fmt "%s" (show_res_kind r)

let pp_ident fmt = function
  | Simple mlv -> Format.fprintf fmt "@[%s@]" (Printing.mlvar_show mlv)
  | Source id -> Ast.Ident.pp fmt id

let rec pp_expr' fmt e =
  let open Format in
  match e with
  | Const c -> Ast.pp_const fmt c
  | Var id -> pp_ident fmt id
  | Res (r, e) -> fprintf fmt "@[<hov 2>%a(%a)@]" pp_res_kind r pp_expr e
  | Proj (r, e) -> fprintf fmt "@[<hov 2>(%a).%a@]" pp_expr e pp_res_kind r
  | IfNotRes {cond;v;s;body} ->
    fprintf fmt "@[@[<hov 2>bind %a, %a =@ %a in@]@ %a@]"
      pp_ident v pp_ident s pp_expr cond pp_expr body
  | Val {cond; v; s; body} ->
    fprintf fmt "@[@[<hov 2>bindv %a, %a =@ %a in@]@ %a@]"
      pp_ident v pp_ident s pp_expr cond pp_expr body
  | Ite (e1, e2, e3) ->
    fprintf fmt
      "@[@[<hov 2>if %a@]@\n@[<hov 2>then %a@]@\n@[<hov 2>else %a@]@]@\n"
      pp_expr e1 pp_expr e2 pp_expr e3
  | Tuple el ->
    fprintf fmt "@[(@[%a@])@]" (Printing.pp_list ~sep:",@ " pp_expr) el
  | Pi (i, e) -> fprintf fmt "@[π%d(%a)@]" i pp_expr e
  | EmptyRec -> fprintf fmt "{}"
  | RecUpdate (e1, id, e2) ->
    fprintf fmt "@[<hov 2>{ %a with@ %a = %a }@]"
      pp_expr e1 pp_ident id pp_expr e2
  | Field (e, id) -> fprintf fmt "@[%a@,.%a@]" pp_expr e pp_ident id
  | DelStateFields(l, e) -> fprintf fmt "@[%a@,\\{%a}@]"
                              pp_expr e (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_ident) l
  | Lambda (id, State ty, e) ->  fprintf fmt "@[<hov 2>ƛ %a:%a.@ %a@]"
                                   pp_ident id
                                   MT.Ty.pp ty
                                   pp_expr e
  | Lambda (id, Normal, e) -> fprintf fmt "@[<hov 2>λ %a.@ %a@]"
                                pp_ident id pp_expr e
  | App (e1, e2) -> fprintf fmt "@[<hov 2>(%a)@ %a@]" pp_expr e1 pp_expr e2
and pp_expr fmt (_,e) = pp_expr' fmt e
