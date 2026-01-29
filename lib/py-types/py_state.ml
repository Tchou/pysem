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
let none = Ast.Const.of_constant (PC.Constant.make_none_of_t ())

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
  let s = mk_ident () in
  pos, Lambda(s, true, pair pos
                (res pos V (pos, Const none))
                (pos, RecUpdate ((pos, Var s),id, e)
                ))

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
  let f = Lambda (x, true,
                  (pos,
                   Lambda (s1, true,
                           (pos,
                            IfNotRes {
                              cond = app pos e
                                  (pos, RecUpdate ((pos, Var s1), x, (pos, Var s1)));
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
  pos, Lambda(s0, true, (pos, IfNotRes {
      cond = app pos e1 (pos, Var s0);
      v = f;
      s = s1;
      body =
        pos, IfNotRes {
          cond = app pos e2 (pos, Var s1);
          v = arg;
          s = s2;
          body = app pos (app pos (pos, Var f) (pos, Var arg)) (pos, Var s2)
        }
    }))
