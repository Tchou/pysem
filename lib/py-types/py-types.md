# Encoding of Python types in set-theoretic types

## Python function signature
This is done in [py_params.ml](py_params.ml).

Given a Python function:
  ```python
  def f (p1 : d1? , …, pn : dn? /,
         mn+1 : dn+1?, …, mdn+k : dn+k?, *,
         kwn+k+1 : dn+k+1?, …, kwn+k+l: dn+k+l? ):
         ...
   ```
  where:
  - the `pi` are positional parameters
  - the `mi` are mixed parameters (can be passed by position or keyword)
  - the `kwi` are keyword only parameters (must be passed as keyword)
  - the `di?` are either absent or of the form `= ei` to denote that a default
    value is provided

Furthermore, there is a syntactic constraint in python that if `di` is present,
then `dj` is also present for all `j>i`.
We pack the signature in a ST type:
```
  %%py_param_spec(record_part, descr_part)
```
that is a tagged type, with a tag `%%py_param_spec` and an underlying type which
is a couple of a `record_part` and a `descr_part`.
The record part encodes function parameters and is used for actually typing the
function. The descr part is a 'descriptor' and is used only for pretty-printing,
since recovering a readable type signature from the record type might be
complicated.

### Record part

Reusing the notation from above, the record part is:
```
  { __1:#'a1, …, __n:#'an,  mn+1:# 'an+1, …  mn+k:# 'an+k, kwn+k+1 :#'an+ +1, … kn+k+l:#'an+k+l  }
| { __1:#'a1, …, __n:#'an, __n+1:# 'an+1, …  mn+k:# 'an+k, kwn+k+1 :#'an+ +1, … kn+k+l:#'an+k+l  }
| …
| { __1:#'a1, …, __n:#'an, __n+1:# 'an+1, … __n+k:# 'an+k, kwn+k+1 :#'an+ +1, … kn+k+l:#'an+k+l  }
```
where `#` means `?` if the python argument has a default value (meaning the
corresponding record has an optional field of that name). The `__i` are dummy
field types that correspond to positional parameter `i`. The `__` part (before
`mn+1`) and the `kw` part after `mn+k` are always fixed. For the center part
(mixed parameters) we have `k` records in total were at record number `i`, the
mixed parameters at index `i` and below are positional and the rest of the mixed
parameters are as keyword.

### Descriptor part

The descriptor is a small data-structure which tells us how to pretty-print
the type. It is encoded as a union of `Enum.t`:

- `%%py_param_approx_spec`: is the default value, it means that the type in the
  left component of the spec (the record) does not come from a signature, but
  was computed (e.g., by type inference, as the result of an application, …). In
  that case it is printed as a union of simple signatures. The record type is
  approximated as a union of positive records. Fields named `__i` contribute to
  positional parameter `i`. Other fields represent keyword parameters.
- `%%py_param_exac%t0123`: for each explicit signature we create an explicit
  descriptor (described afterward) and a unique `Enum.t` which is used as a key
  in a hash-table.

For the record above, the explicit descriptor would be:
```ocaml
 [ Anon (1, b1); Anon (2, b2); … ; Str "/";
   Named ("mn+1", bn+1); …       ; Str "*";
   Named ("kw+n+l+1", bn+k+1); …
 ]
```
It's then sufficient to print the list in order the `bi` are boolean which
records whether a default value was present. The type is pretty-printed by
extracting the corresponding field from the record.

### Example

Consider the function:
```python
def f(g):
    return (g(1,2,z=4),g(u=1, v=2))
```
`f` is deduced to have type:
```
f: [X, Y](g: ((u=1,  v=2) -> Y) & ((1,  2,  z=4) -> X)) -> (X, Y)
```
The type of `f` is an exact signature, it is encoded as:
```
py_param_spec(R, %%py_param_approx_spec | %%py_param_exact_spec123)
```
The enum `%%py_param_exact_spec123` refers (in a hash-table) to a descriptor:
```ocaml
  [ Named ("g", false) ]
```
The type of `g` on the other hand is encoded as:
```
py_param_spec(R', %%py_param_approx_spec)
```
so the DNF of `R'` is computed, and the type printed as a union of intersection.

### Default arguments

When default arguments are recorded, the `pylint` compatible syntax is used:
```python
 (int = ..., /, u : bool = ... ) -> str
```
means that this function has arity 2, one positional parameter and one mixed,
and both have a default value.

### Type Schemes and gradual types

Type schemes are written:
```
['X, 'Y](x: 'X, y: 'Y) -> 'X | 'Y
```
where the `['X, 'Y]` acts as a binder for the names `'X` and `'Y`.

Gradual type schemes are written as:
```
inf <: Any <: sup
```
where `inf` and `sup` are the lower and upper bound of the gradual type and
`Any` is the gradual type (`?` in the literature of set-theoretic-types).
