type str = TODO
type int = TODO
type _NoneType = TODO
type _None = TODO
type bool = TODO
type _NotImplemented = TODO
type tuple = TODO
type list_str = TODO

type base ('self) = (self where self = 'self & {
    __repr__ : self -> str; (* https://docs.python.org/3/reference/datamodel.html#object.__repr *)

    __hash__ : self -> int; (* https://docs.python.org/3/reference/datamodel.html#object.__hash
                               can also be None to make object non Hashable *)

    __str__ : self -> str;  (* https://docs.python.org/3/reference/datamodel.html#object.__str *)

    __getattribute__ : (self, str) -> object;  (* (name: str)
                                                  https://docs.python.org/3/reference/datamodel.html#object.__getattribute *)
      __setattr__ : (self, str, object) -> _None; (* (name: str, value:object)
                                                     https://docs.python.org/3/reference/datamodel.html#object.__setattr
                                                  *)

        __delattr__ : (self, str) -> _None;  (* (name:str)
                                                https://docs.python.org/3/reference/datamodel.html#object.__delattr
                                             *)

        __eq__ : (self, object) -> bool | _NotImplemented;  (* https://docs.python.org/3/reference/datamodel.html#object.__eq *)
        __ne__ : (self, object) -> bool | _NotImplemented;
        __lt__ : (self, object) -> bool | _NotImplemented;  (* https://docs.python.org/3/reference/datamodel.html#object.__lt *)
        __le__ : (self, object) -> bool | _NotImplemented;
        __gt__ : (self, object) -> bool | _NotImplemented;
        __ge__ : (self, object) -> bool | _NotImplemented;
        __reduce_ex__ : (self, int) -> tuple; (* https://docs.python.org/3/library/pickle.html#object.__reduce_ex *)
        __reduce__ : self -> tuple;
        __getstate__ : self -> object;  (* https://docs.python.org/3/library/pickle.html#object.__getstate *)
        __format__ : (self, str) -> str;  (* https://docs.python.org/3/reference/datamodel.html#object.__format *)
          __sizeof__ : self -> int; (* https://docs.python.org/3/reference/datamodel.html#object.__sizeof *)
          __doc__ : str | _None; (* https://docs.python.org/3/reference/datamodel.html#the-standard-type-hierarchy *)
          __dir__ : self -> list_str; (* https://docs.python.org/3/reference/datamodel.html#object.__dir *)
          __subclasshook__ : object -> bool | _NotImplemented
                                    (* __new__     variadic
                                       __init__     variadic
                                       __init_subclass__
                                    *)

                                    .. } )
and object ('self)= base('self)


(*
class A:  # == A(object)

   def f (self, x, y, /):
       return self.z + x + y

*)
val init_obj : base('a)

val (+) : (int, int) -> int


let mkA (_x:()) =
  let f arg =
    let (self, x) = arg in
    let _x = (self :> base('a)) in
    self.z + x
  in
  {
    init_obj
    with
      f = f
  }


