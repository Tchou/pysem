open Aliases

type res_kind = R | V
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
  | Lambda of ident * bool * expr (* true ⇒ synthetic *)
  | App of expr * expr
and expr = MC.Position.t * expr'

let mlvar = function
    Simple v -> v
  | Source(Ast.{name; _ }) -> name

let subst e s =
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
    | Lambda (id, b, e) ->  Lambda(id, b, loop e)
    | App (e1, e2) -> App (loop e1, loop e2)
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
         |exception Not_found -> snd e)
      else snd e
    | Lambda (id, b, e) ->  Lambda(id, b, loop e)
    | App(e1, e2) ->
      let e1 = loop e1 in
      let e2 = loop e2 in
      match snd e1 with
      | Lambda(id, true, e) -> snd (loop (subst e2 [id, e]))
      | _ -> App (e1, e2)
  in loop e

let mk_ident () = Simple (MlVar.create None)
let pair pos e1 e2 =
  pos, Tuple[e1; e2]

let res pos k e =
  pos, Res(k, e)

let app pos e1 e2 =
  pos, App(e1, e2)
let none = Ast.None_

(* Combinators *)
let const pos c =
  let s = mk_ident () in
  pos, Lambda (s, true, pair pos
                 (res pos V (pos, Const c))
                 (pos, Var s) )

let var_get pos id =
  let s = mk_ident () in
  pos, Lambda (s, true, pair pos
                 (res pos V (pos, Field ((pos, Var s), id)))
                 (pos, Var s))

let var_set pos id e =
  let s0 = mk_ident ()
  and s1 = mk_ident () in
  let v = mk_ident () in
  pos, Lambda(s0, true,
              (pos, IfNotRes {cond=app pos e (pos, Var s0);v;s=s1;
                        body=pair pos
                            (res pos V (pos, Const none))
                            (pos, RecUpdate ((pos, Var s1),id, (pos, Var v))
                            )})
                )

let return pos e =
  let v = mk_ident () in
  let s = mk_ident () in
  let s0 = mk_ident () in
  pos, Lambda(s0, true, (pos,
                         IfNotRes{cond = app pos e (pos, Var s0);
                                  v; s;
                                  body=
                                    pair pos
                                      (res pos R (pos, Var v))
                                      (pos, Var s) }))

let ite pos cond e1 e2 =
  let s0 = mk_ident () in
  let v = mk_ident () in
  let s = mk_ident () in
  let ps = pos, Var s in
  pos, Lambda(s0, true,
              (pos, IfNotRes { cond = app pos cond (pos, Var s0); v; s;
                               body = pos, Ite ((pos, Var v), app pos e1 ps,
                                                app pos e2 ps)
                             }))

let seq pos e1 e2 =
  let s0 = mk_ident () in
  let v = mk_ident () in
  let s = mk_ident () in
  pos, Lambda(s0, true,
              (pos, IfNotRes {
                  cond = app pos e1 (pos, Var s0); v; s;
                  body = app pos e2 (pos, Var s);
                }
              )           )

let lambda pos x e =
  let s1  = mk_ident () in
  let v = mk_ident () in
  let s = mk_ident () in
  let f =
    Lambda
      (x, false,
       (pos, Lambda
          (s1, true,
           (pos, IfNotRes {
               cond = app pos e
                   (pos, RecUpdate ((pos, Var s1), x, (pos, Var (s1))));
               v;
               s;
               body = pair pos (res pos V (pos, Const none)) (pos, Var s);
             }
           )))
      )
  in
  let s0 = mk_ident () in
  pos, Lambda(s0, true, pair pos (res pos V (pos,f)) (pos, Var s0))

let apply pos e1 e2 =
  let s0 = mk_ident () in
  let f = mk_ident () in
  let s1 = mk_ident () in
  let arg = mk_ident () in
  let s2 = mk_ident () in
  let v = mk_ident () in
  let s3 = mk_ident () in
  pos, Lambda(s0, true, (pos, IfNotRes {
      cond = app pos e1 (pos, Var s0);
      v = f;
      s = s1;
      body =
        pos, IfNotRes {
          cond = app pos e2 (pos, Var s1);
          v = arg;
          s = s2;
          body =
            pos, Val {
              cond = app pos (app pos (pos, Var f) (pos, Var arg))
                  (pos, Var s2);
              v;
              s = s3;
              body = pair pos (res pos V (pos, Const none)) (pos, Var s3)
            }
        }
    }))

let tuple2 pos e1 e2 =
  let s0 = mk_ident ()
  and s1 = mk_ident ()
  and s2 = mk_ident () in
  let v1 = mk_ident ()
  and v2 = mk_ident () in
  pos, Lambda (s0, true, (pos, IfNotRes {
      cond = app pos e1 (pos, Var s0);
      v = v1; s = s1;
      body = pos, IfNotRes {
          cond = app pos e2 (pos, Var s1);
          v = v2;
          s = s2;
          body = pair pos (pos, Var v1) (pos, Var v2)}}))

(* translation *)

let of_opt p eo f = match eo with
  | None -> const p none
  | Some e -> f e

let rec of_expr (p,e:Ast.expr) : expr = match e with
  | Var id -> (* λs.V(s.x),s *)
    var_get p (Source id)
  | Binop _ ->
    (* λs0. bind v1, s1 = [e1] s0 in
            bind v2, s2 = [e2] s1 in
            bind op, s3 = [op] s2 in
            ((op e1) e2) s3 (??) *)
    failwith "TODO Binop"
  | Cst c -> (* λs.V(c),s *)
    const p c
  | Lambda (spec,_,body) ->
    let x = of_spec spec in
    let e = of_expr body in
    lambda p x e
  | Apply (e,param) ->
    (* λs0. bind f, s1 = [e] s0 in
            bind x, s2 = [p] s1 in
            (f x) s2 *)
    let e1 = of_expr e
    and e2 = of_param param in
    apply p e1 e2
  | Tuple [e1;e2] ->
    let e1 = of_expr e1
    and e2 = of_expr e2 in
    tuple2 p e1 e2
  | Tuple _ ->
    (* λs0. bind x1, s1 = [e1] s0 in ... bind xn, sn = [en] sn-1 in
            (x1, ..., xn), sn *)
    failwith "TODO all tuples"

and of_spec {posonly;_} = match posonly with
  | [ (x,None) ] -> Source x
  | _ -> failwith "TODO not just 1 posonly"

and of_param {pos;_} = match pos with
  | [] -> Var (mk_ident ()) |> Ast.dannot
  | [ e ] -> of_expr e
  | _ -> failwith "TODO not just 1 pos parameter"

let rec of_instr (p,i:Ast.instr) = match i with
  | Block il ->
    begin match List.map of_instr il with
      | [] -> const p none
      | e1::l -> List.fold_left (fun sq ((p,_) as e) -> seq p sq e ) e1 l
    end
  | FunDef (id, spec, _, body)  ->
    let x = of_spec spec in
    let e = of_instr body in
    var_set p (Source id) (lambda p x e)
  | Return eo ->
    of_opt p eo of_expr
    |> return p
  | Assign (tg, e) ->
    var_set p (Source tg) (of_expr e)
  | While (_e, _i) -> failwith "TODO while control flow"
  | If (test, i, io) ->
    ite p
      (of_expr test)
      (of_instr i)
      (of_opt p io of_instr)
  | Iexpr e -> of_expr e
  | Break | Continue -> failwith "TODO while control flow"

let of_prog p = List.map of_instr p


let ident_name id = mlvar id |> Printing.mlvar_show

let res_tag = function
  | R -> MT.Tag.define "R"
  | V -> MT.Tag.define "V"

let rec to_ml (p,e) =
  let open Utils in
  match e with
  | Const c -> mk_value p Ast.Const.(to_gty c)
  | Var id -> mlvar id |> var_of_vart p
  | Res (r, r_e) -> mk_tag p (res_tag r) (to_ml r_e)
  | Proj (r, p_e) -> mk_proj_tag p (res_tag r) (to_ml p_e)
  | IfNotRes _ -> failwith "TODO bind_value"
  | Val _ -> failwith "TODO bind_return"
  | Ite (test, e1, e2) ->
    mk_ite p (to_ml test) MT.(GTy.mk Ty.tt) (to_ml e1) (to_ml e2)
  | Tuple el -> mk_tuple p List.(map to_ml el)
  | Pi (i,e) -> mk_proj_tuple p i 2 (to_ml e) (* FIXME arity 2 hard coded! *)
  | EmptyRec -> mk_record p [] []
  | RecUpdate (_r, x, e) -> (* FIXME how to use MLAst.Operation ? *)
    mk_record_update p
      (ident_name x)
      (to_ml e)
  | Field (e, id) -> mk_projection p (MSAst.PiField (ident_name id)) (to_ml e)
  | Lambda (id,_,e) ->
    mk_lambda p [] MT.(TVar.(mk KInfer (Some "??") |> typ)|> GTy.mk)
      (mlvar id) (to_ml e)
  | App (e1, e2) -> mk_app p (to_ml e1) (to_ml e2)

(* Print *)

let show_res_kind = function R -> "R" | V -> "V"
let pp_res_kind fmt r = Format.fprintf fmt "%s" (show_res_kind r)

let pp_ident fmt = function
  | Simple mlv -> Format.fprintf fmt "@[%%%s@]" (Printing.mlvar_show mlv)
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
  | Lambda (id, b, e) ->
    fprintf fmt "@[<hov 2>%s %a.@ %a@]"
      (if b then "ƛ" else "λ") pp_ident id pp_expr e
  | App (e1, e2) -> fprintf fmt "@[<hov 2>(%a)@ %a@]" pp_expr e1 pp_expr e2
and pp_expr fmt (_,e) = pp_expr' fmt e
