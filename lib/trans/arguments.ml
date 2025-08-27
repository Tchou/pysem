(** Translation of function parameters and function calls *)
open PyreAst.Concrete
open Mlsem
(*
Reference:
https://docs.python.org/3/reference/compound_stmts.html#function-definitions

f (a,b,c=42,/,*,kw1=1,kw2=2)
f (1,2, kw2=33)

f  (/,x,y,z=33,* )

General scheme. Given an [Arguments.t] record with:
{
posonlyargs : p1=?def1, …, pk=?defk
/
args:         a1=?defk+1, …, al=?defl   , where if defi in defaults.
                                         if defi is present, then it must by present
                                         for all j > i. So to associate a default value
                                         to a parameter, one only needs to join them from
                                         the end of both lists.
*
vararg         *va                       rest of positional parameters
kwonlyargs:    kw1=?dkw1, …, kwm=?dkwm   dkwi from kw_defaults. Boths lists have the same
                                         length, with None value∩s if no kwargs.

kwarg          *kw                       rest of the kw arguments
}

we map to a record type:
{  #1:?X1, … ,#k:?Xk  .. }                 pos or keyword
&
( { #k+1:?Xk+1, … #l:?Xl ..}      
| { #k+1:?Xk+1,…, al:?Xl ..}
| …
| { a1:?Xk+1, …, al:?Xl ..})
&
{  kw1:?Y1, … ,kwm:?Xm .. }  
&
{  #arity:(k,l-k+m)|(k+1,l-k-1+m)|…|(l,m) ..}

a call
f (v1, …, vn,x1=v1,…,xm=vm )
is turned into:
f ({#1:v1, … #n:vn; x1:v1; … ; xm:vm; #arity:(n,m)})


the body of f is then turned into:
let def1 = e_def1 in
let def2 = e_def2 in … (* define default expression outside, they are evaluated only once *)
let f r =
    let p1 = if r in {#1:any} then r.#1 else def1 in (*if #r1 is optional*)
    let p2 = r#2 in                                  (* otherwise *)
    ...
    let a1 = if r in {#k+1:any} then r.#k+1 else if r in { a1 : any} then r.a1 else defk+1 in
    ...
    let kw1 = if r in {kw1:any} then r.kw1 else dkw1 in
    (* todo later va and kwargs*)
*)

let zip_for l1 l2 =
  let rec loop l1 l2 acc =
    match l1, l2 with
      [], l2 -> acc, l2
    | e1 :: ll1, e2 :: ll2 -> loop ll1 ll2 ((e1, Some e2)::acc)
    | e1 :: ll1, [] -> loop ll1 [] ((e1, None)::acc)
  in
  loop l1 l2 []

let join_slide l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
      [], _ -> [List.rev acc,[]]
    | e1 :: ll1, _::ll2 ->
      (List.rev acc, l2) :: loop (e1::acc) ll1 ll2
    | _ -> assert false
  in
  loop [] l1 l2

let pos_param_name i = Utils.mk_id "#%d" i
let kw_param_name kw = Utils.mk_id ":%s" kw

type proto = {
  type_ : Types.Ty.t list;
  pos_only : (Argument.t * int * Expression.t option * Types.TVar.t) list;
  args : (Argument.t * int * Expression.t option * Types.TVar.t) list;
  kw_only : (Argument.t * int * Expression.t option * Types.TVar.t) list;
}

let pp_arg fmt (a, i, eo, v) =
  let open Format in
  fprintf fmt "(%s, %d, %a, %a)"
    (Identifier.to_string a.Argument.identifier)
    i
    (pp_print_option Utils.pp_expr) eo
    Types.TVar.pp v

let pp_arg_list fmt l =
  let open Format in
  pp_print_list  ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") pp_arg fmt l

let pp_proto fmt p =
  let open Format in
  fprintf fmt "@[type: @[%a@]@\n" (pp_print_list ~pp_sep:pp_print_space Types.Ty.pp) p.type_;
  fprintf fmt "check_type: @[%a@]@\n" Types.Ty.pp Types.Ty.(conj p.type_ |> Types.Ty.simplify);
  fprintf fmt "pos_only: @[%a@]\n" pp_arg_list p.pos_only;
  fprintf fmt "args: @[%a@]@\n" pp_arg_list p.args;
  fprintf fmt "kw_only: @[%a@]@]@\n" pp_arg_list p.kw_only

let interval i j = Types.Ty.interval (Some (Z.of_int i)) (Some (Z.of_int j))
let translate_arguments (a : Arguments.t) =
  let open Types in
  let args, rem_init = zip_for (List.rev a.args) (List.rev a.defaults) in
  let pos_only, rem_pos_only = zip_for (List.rev a.posonlyargs) rem_init in
  assert (rem_pos_only = []);
  let kw_only = List.map2 (fun a b -> (a,b)) a.kwonlyargs a.kw_defaults  in
  let mk_var i = TVar.(mk KInfer (Some (Utils.mk_id "#%d" i))) in
  let add_vars l n = List.mapi (fun i (a, eo) -> a, i+n, eo, mk_var (i+n)) l in
  let pos_only = add_vars pos_only 0 in
  let len_pos_only = List.length pos_only in
  let len_args = List.length args in
  let args = add_vars args len_pos_only in
  let kw_only = add_vars kw_only (len_pos_only + len_args) in
  let mk_pos_fields = List.map (fun (_a, idx, eo, v) ->
      pos_param_name idx,
      (Option.is_some eo, TVar.typ v)
    )
  in
  let mk_kw_fields = List.map (fun (a, _, eo, v) ->
      kw_param_name (Identifier.to_string a.Argument.identifier),
      (Option.is_some eo, TVar.typ v)
    )
  in
  let pos_only_fields = mk_pos_fields pos_only in
  let kw_only_fields = mk_kw_fields kw_only in
  let pos_args_fields = mk_pos_fields args in
  let kw_args_fields = mk_kw_fields args in
  let args_recs = 
    (* order matters, join_slide will generate by increasing pos *)
    join_slide pos_args_fields kw_args_fields
  in
  let count_min = Utils.count_if (fun (_,(b, _)) -> not b) in
  let min_pos =  count_min pos_only_fields in
  let max_pos = List.length pos_only_fields in
  let min_kw = count_min kw_only_fields in
  let max_kw = List.length kw_only_fields in

  let type_ = List.map (fun (pos_part, kw_part) ->
      let min_pos = min_pos + count_min pos_part in
      let max_pos = max_pos + List.length pos_part in
      let min_kw = min_kw + count_min kw_part in
      let max_kw = max_kw + List.length kw_part in
      let pos = interval min_pos max_pos in
      let kw = interval min_kw max_kw in

      Tuple.(mk [ mk [ pos; kw ] ;
                  Record.mk true (pos_only_fields @ pos_part @ 
                                  kw_part @ kw_only_fields)])
    ) args_recs 
  in
  let () = Format.eprintf ">> %d\n%!" (List.length type_) in

  { type_;
    pos_only;
    args;
    kw_only}



    (*

    let defc = fib(42) in

    let f (_, r) = 
      let a = r.#0 in
      let b = r.#1 in
      let c = if r is { #2: Any; ..} ? r.#2 : defc in




    *)