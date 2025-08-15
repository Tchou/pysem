type ident_kind = Global | Nonlocal | Local
module IdentMap = Map.Make(PyreAst.Concrete.Identifier)


let translate_body (_env : Env.t) _fun_info _ = assert false


(* Function definitions:
   def f(p1=ep1,…, pl=epl /, r1=er1,…, rm=erm, *args, *, k1=ek1,…, kn=ekn, **kwargs):
    ...
   Encoding:
   let def_ep1 = ep1 in
   …
   let def_ekn = ekn in
   let f (r : { _1; ... ; _l; k1; …; kn … ; r1; … ; rm } |
              { _1; ... ; _l; k1; …; kn … ; _l+1; r2; …; rm } |
              { _1; ... ; _l; k1; …; kn … ; _l+1; _l+2; …; rm } |
              …
              { _1; ... ; _l; k1; …; kn … ; _l+1; _l+2; …; _l+m }, args: any::any::…::any::xargs, kwargs) =
    let p1 = r in {_1 : any .. } ? r._1 : def_ep1 in
    let p2 = r._2 in
    …
    let k1 = r in { kw1 : any .. } ? r.kw1 : def_kw1 in
    …
    let r1 = r in { r1 : any .. } ? r.r1 : r in { _l+1: any .. } ? r._l+1 : def_r1 in



   Funcion calls:

   f(e1, …,ej,kw1=e1,...,kwk=ek)
   let v1 = e1 in
   …
   let wk = ek in

   f({ _1 : v1; …; _j = vj; kw1=w1,…, kwk=wk},[v1;…;vj], )


*)
let function_def ~toplevel ~location ~name ~args ~body ~decorator_list ~returns ~type_params =
  ignore toplevel;
  ignore location;
  ignore name;
  ignore args;
  ignore body;
  ignore decorator_list;
  ignore returns;
  ignore type_params;
  assert false

let toplevel (env : Env.t) s =
  let open PyreAst.Concrete.Statement in
  match s with
    FunctionDef { location; name; args; body; 
                  decorator_list; 
                  returns; type_params; 
                  _ (* type_comments, ignore since Py2 style *)
                } -> function_def ~toplevel:true ~location ~name ~args ~body ~decorator_list ~returns ~type_params
  | AsyncFunctionDef { location; _ }
  | ClassDef { location; _ }
  | Return { location; _ }
  | Delete { location ; _  }
  | Assign { location; _ }
  | TypeAlias { location; _ }
  | AugAssign { location; _ }
  | AnnAssign { location; _ }
  | For { location; _ }
  | AsyncFor { location; _ }
  | While { location; _ }
  | If { location; _ }
  | With { location; _ }
  | AsyncWith { location; _ }
  | Match { location; _ }
  | Raise { location; _ }
  | Try { location; _ }
  | TryStar { location; _ }
  | Assert { location; _ }
  | Import { location; _ }
  | ImportFrom {location; _ }
  | Global { location; _ }
  | Nonlocal { location; _ }
  | Expr { location; _ }
  | Pass { location; _ }
  | Break { location; _ }
  | Continue { location; _ } -> Error.unimplemented env.filename location