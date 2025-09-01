(* MLsem tests, not OCaml code *)

(*
Sandbox for python encoding into types.
Naming:

foo_type : is the MLsem type representing Python object named
foo, that have python type 'type'. So the Python object
  'int' 'bool' 'list' etc… are represented by MLsem values
  of type 'int_type', 'bool_type' 'list_type'

foo_val ('a) : is the MLsem type representing Python object of
python type foo. So the python value '42' has type 'int_val'
The python value '(1,2)' has type 

*)


type meta_dict ('b) = Dict({ __get : string -> any .. } & 'b)

(* The type of type, the sstt type for python's type object. To 
   bootstrap everything, we define monomorphic versions first
*)
type type_type = Py({ __dict__ : () -> meta_dict({ ..});
                   __class__ : () -> type_type;
                   __mro__ : () -> type_type_mro;
                   __ty__ : () -> Type  .. })
and type_type_mro = Py({
  __dict__ : () -> meta_dict ({ ..});
  __class__ : () -> tuple_type;
  __ty__ : () -> [ type_type  object_type ] .. })
and tuple_type = Py({
  __dict__ : () -> meta_dict ({ ..});
  __class__ : () -> type_type;
  __mro__ : () -> tuple_type_mro; 
  __ty__ : () -> Type    .. })
and tuple_type_mro = Py({
  __dict__ : () -> meta_dict ({ ..});
  __class__ : () -> tuple_type;
  __ty__ : () -> [ tuple_type  object_type ] .. })
and object_type = Py({
  __dict__ : () -> meta_dict({ ..});
  __class__ : () -> type_type;
  __mro__ : () -> object_type_mro; 
  __ty__ : () -> Type .. })
and object_type_mro = Py({
  __dict__ : () -> meta_dict ( { ..});
  __class__ : () -> tuple_type;
  __ty__ : () -> [ object_type ] .. })

(* polymorphic type interface  *)
type object ('mro, 'att, 'ty) = Py({
  __dict__ : () -> meta_dict ('att);
  __class__ : () -> type_('mro);
  __ty__ : () -> 'ty
.. })
and type_ ('mro) = Py({
  __dict__ : () -> meta_dict ({ ..});
  __class__ : () -> type_type;
  __mro__ : () -> tuple ({ ..}, 'mro);
  __ty__ : () -> Type
.. })
and tuple ('att, 'ty) = Py({
  __dict__ : () -> meta_dict ('att);
  __class__ : () -> tuple_type;
  __ty__ : () -> 'ty
.. })


type direct_x ('a) = object (any, { x : 'a .. }, any)
type x_in_mro ('a) = [ (~ direct_x (any))* (direct_x ('a))  type_type* ]
type attr_x ('a) = direct_x ('a) | object (x_in_mro('a), { .. }, any)
val get_x : attr_x ('a) -> 'a

type direct_y ('a) = object (any, { y : 'a .. }, any)
type y_in_mro ('a) = [ (~ direct_y (any))* (direct_y ('a))  type_type* ]
type attr_y ('a) = direct_y ('a) | object (y_in_mro('a), { .. }, any)
val get_y : attr_y('a) -> 'a

let f a = (get_x a) + (get_y a)

