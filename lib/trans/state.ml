open Mlsem
type 'a ctrl =
    Normal of 'a
  | Return  of 'a
  | Break
  | Continue
  | Throw of 'a
  | Yield of 'a

module StringMap = Map.Make(String)

type 'a state = { 
  ctrl : 'a ctrl;
  local : Types.Ty.t StringMap.t;
  nonlocal : Types.Ty.t StringMap.t;
  global : Types.Ty.t StringMap.t;
}

(*
Encoding of object :
type object = {
   __new__ :  (object) => object;
   __repr__ : (object) => str;
   __hash__ : (object) => int;
   __str__ : (object) => str;
   __getattribute__ : (object) => object
   __setattr__ : (object, str, object,/) => None;
   __delattr__ : (object, str, /) => None;
   __lt__ : (object, object) => bool;
   ...
   __init__ : (object,/) => None;
   __format__ : (object, str, /) => str;
   __sizeof__ : (object) => int;
   __dir__ : (object) => list (str)
   __class__ : (object) => type;
   __doc__ : str;
   __name__: 'object';
   __qualname__ : 'object';
   __module__ : 'bultins';
   __mro__ : [ object ];
   mro : () => list(type);
   __bases__: [];
   __base__ : None;
 }
and int = {
  
  }




*)


module Builtins =
struct
  module Types = struct
    
  end
end