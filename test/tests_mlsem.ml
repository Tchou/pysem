(* MLsem tests, not OCaml code *)
(* Get an attribute x from an object *)

type meta_dict ('b) = { get : string -> any .. } & 'b

(* The type of type, the sstt type for python's type object *)
type type_type = (type_ where type_ = { __dict__ : () -> meta_dict({ ..});
                                        __class__ : () -> type_;
                                        __mro__ : () -> mro_of_type .. }
                  and mro_of_type = {
                    __dict__ : () -> meta_dict ({ ..});
                    __class__ : () -> tuple_type;
                    __tuple_content__ : () -> [ type_  object_type ] .. }
                  and tuple_type = {
                    __dict__ : () -> meta_dict ({ ..});
                    __class__ : () -> type_;
                    __mro__ : () -> mro_of_tuple .. }
                  and mro_of_tuple = {
                    __dict__ : () -> meta_dict ({ ..});
                    __class__ : () -> tuple_type;
                    __tuple_content__ : () -> [ tuple  object_type ] .. }
                  and object_type = {
                    __dict__ : () -> meta_dict({ ..});
                    __class__ : () -> type_;
                    __mro__ : () -> mro_of_object .. }
                  and mro_of_object = {
                    __dict__ : () -> meta_dict ( { ..});
                    __class__ : () -> tuple_type;
                    __tuple_content__ : () -> [ object_type ] .. })

(* the sstt type for tuple *)

type tuple_type = (tuple_type where type_ = { __dict__ : () -> meta_dict({ ..});
                                              __class__ : () -> type_;
                                              __mro__ : () -> mro_of_type .. }
                   and mro_of_type = {
                     __dict__ : () -> meta_dict ({ ..});
                     __class__ : () -> tuple_type;
                     __tuple_content__ : () -> [ type_  object_type ] .. }
                   and tuple_type = {
                     __dict__ : () -> meta_dict ({ ..});
                     __class__ : () -> type_;
                     __mro__ : () -> mro_of_tuple .. }
                   and mro_of_tuple = {
                     __dict__ :  () -> meta_dict ({ ..});
                     __class__ : () -> tuple_type;
                     __tuple_content__ : () -> [ tuple  object_type ] .. }
                   and object_type = {
                     __dict__ : () -> meta_dict({ ..});
                     __class__ : () -> type_;
                     __mro__ : () -> mro_of_object .. }
                   and mro_of_object = {
                     __dict__ : () -> meta_dict ( { ..});
                     __class__ : () -> tuple_type;
                     __tuple_content__ : () -> [ object_type ] .. })


(* the type of a polymorphic tuple *)
type tuple ('c) = { __dict__ : () -> meta_dict ( { ..});
                    __class__ : () -> tuple_type;
                    __tuple_content__ : () -> ('c & [any*]) .. }

(* type of a polymorphic type *)
type type_ ('c) = { __dict__ : () -> meta_dict ( { ..});
                    __class__ : () -> type_type; 
                    __mro__ : () -> tuple('c)   ..}
(* type representing a python object with direct attributes 'a and 
   mro :'c 
*)
type object('a, 'c) = { __dict__ : () -> meta_dict('a); 
                        __class__: () -> type_('c)  .. }
type any_object = object ({ ..}, [type_type+])

type direct_x ('a) = object ({ x : 'a .. }, [type_type+])
type x_in_mro ('a) = [ (~ direct_x (any))* (direct_x ('a))  type_type* ]
type attr_x ('a) = direct_x ('a) | object ({ .. }\{x : any ..}, x_in_mro('a))
val get_x : attr_x ('a) -> 'a

type direct_y ('a) = object ({ y : 'a .. }, [type_type+])
type y_in_mro ('a) = [ (~ direct_y (any))* (direct_y ('a))  type_type* ]
type attr_y ('a) = direct_y ('a) | object ({ .. }\{y : any ..}, y_in_mro('a))
val get_y : attr_y('a) -> 'a

let f a = get_x a

(* set field *)
let g (self : any_object) = 
  let d = { (self.__dict__ ()) with c = 42 } in
  let f (_ : ()) = d in
  { self with __dict__ = f }