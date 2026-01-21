# Encoding of Python types in set-theoretic types

## Python function signature
This is done in [py_params.ml](py_params.ml).

Given a Python function:
  ```python
  def f (p1 : d1? , .., pn : dn? /,
         mn+1 : dn+1?, …, mdn+k : dn+k?, *,
         kwn+k+1 : dn+k+1?, …, kwn+k+l: dn+k+l? ):
         ...
   ```
  where:
  - the `pi` are positional parameters
  - the `mi` are mixed parameters (can be passed by position or keyword)
  - the `kwi` are keyword only parameters (must be passed as keyword)
  - the `di?` are either absent or of the form `= ei` to denote that a default value is provided

Furthermore, there is a syntactic constraint in python that
if `di` is present, then `dj` is also present for all `j>i`.
We pack the signature in a ST type:
```
  %%py_param_spec(record_part, descr_part)
```
that is a tagged type, with a tag `%%py_param_spec` and an underlying type which
is a couple of a `record_part` and a `descr_part`
The record part encodes function parameters and is used for actually typing the function. The descr part is a 'descriptor' and is used only for pretty-printing, since recovering a readable type signature from the record type might be complicated.

### Record part
Reusing the notation from above, the record part is:
```
  { __1:#'a1, …, __n:#'an, mn+1:# 'an+1, … mn+k:# 'an+k, kwn+k+1 :#'an+ +1, … kn+k+l:#'an+k+l  }

| { __1:#'a1, …, __n:#'an, __m__n+1:# 'an+1, … mn+k:# 'an+k, kwn+k+1 :#'an+ +1, … kn+k+l:#'an+k+l  }
| …
| { __1:#'a1, …, __n:#'an, __m__n+1:# 'an+1, … __n+k:# 'an+k, kwn+k+1 :#'an+ +1, … kn+k+l:#'an+k+l  }
```
where `#` means `?` if the python argument has a default value (meaning the
corresponding record has an optional field of that name). The `__i` are dummy
field types that correspond to positional parameter `i`. The `__` part (before
m1) and the `kw` part after `mn+k` are always fixed. For the center part (mixed
parameters) we have `k` records in total were at record number `i`, the mixed
parameters at index `i` and below are positional and the rest of the mixed
parameters are as keyword.

### Descriptor part

The descriptor is a small data-structure which tells us how to pretty-print
the type. It is encoded as a union of `Enum.t`:
- `%%py_param_approx_spec`: is the default value, it means that the type in the
left component of the spec (the record) does not come from a signature, but was
computed (e.g., by type inference, as the result of an application, …). In that
case it is printed as a union of simple signatures. The record type is
approximated as a union of positive records. Fields named `__i` contribute
to positional parameter `i`. Other fields represent keyword parameters.

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
It's then sufficient to print the list in order the `bi` are boolean which records
whether a default value was present. The type is pretty-printed by extracting the
corresponding field from the record.

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
The type of `g` on the other end is encoded as:
```
py_param_spec(R', %%py_param_approx_spec)
```
so the DNF of R' is computed, and the type printed as a union of intersection.

### Default arguments
When default arguments are recorded, the `pylint` compatible syntax is used:
```python
 (int = ..., /, u : bool = ... ) -> str
```
means that this function has arity 2, one positional parameter and one mixed,
and both have a default value.

### Type Schemes and gradual types

Type scheme are written:
```
[X, Y](x: X, y: Y) -> X|Y
```
where the `[X, Y]` acts as a binder for the names `X` and `Y`.

Gradual type schemes are written as:
```
[X, Y] inf <: Any <: sup
```
where `inf` and `sup` are the lower and upper bound of the gradual type and
`Any` is the gradual type (`?` in the literature of set-theoretic-types).



## Monadic transformation

### Introduction

We want to transform an imperative Python program in a purely functional one.
In Python, a variable can be global, non-local or local. This information is
syntactic and is computed on the AST just after parsing. We therefore restrict
ourselves to the following language:
```
e ::= c | x⊙ | x⊙ = e | λ<G,N,L>x.e | e e | e; e | if e then e else e | return<L> e
```
where ⊙ is a variable scope and ranges over Ⓖ, Ⓝ, Ⓛ to denote that the symbol
is global, non-local or local.
Lambdas are annotated with the set of variables used *directly* in their body and
return expressions are annotated with the set of local variables that go out of scope. Last, we assume term variables to be unique, and we call `'_a_x` a unique type unification variable associated to variable `x`.

For instance:
```
fⒼ = λ<{A,B},{},{y,g}> x.
     yⓁ = AⒼ + BⒼ;
     gⓁ = λ<{},{y},{u}> z.
                        uⓁ = 42
                        yⓃ = uⓁ + zⓁ
                        return<{u,z}> yⓃ
     return<{x,y,g}> gⓁ
```
In the example above, at top level, the lambda stored in `f` uses two global
variables (`A` and `B`), and two local variables (`y` and `g`). `g` is itself a
lambda which references no global variable, a non-local variable `y` and a local
variable `u` (function parameters, here `x` and `z` are in the local scope).
Note that in the example above, it does not matter that `A` and `B` are not defined, it suffices that their definition precedes any call to `f`.
Another problem here (that the encoding below addresses) is that when `f`
is called, it returns a closure of `g` where the non local environment is "closed": it should not expect a non-local environment containing `y` when it is called. More generally, the problem is precisely the following:
Given a closure `λ<G,N,L> x.e`, we observe that:
- the variables in `L` are created and discarded every time the closure is evaluated, no problem
- the variables in `G` are always in scope (and it's an error if we call the function when they are not defined)
- the variables in `N` are problematic. If we call the closure, outside of their scope of definition, we cannot access them anymore.

So we must ensure that when we create a closure, it will never require a variable from the local scope to be present and will not use it.

### Encoding

We present an encoding which is a combination of:
- state passing style (modified to handle the non-local and local scopes)
- the exception monad (to handle the 'return' and can easily be extended to handle exceptions)
The rewriting of a mini-python expression `⟦e⟧<G,N,L>` is an MLSem term
```λs. (r, s')``` where:
- `s` is a triple of records representing the global, nonlocal and local state
- `r` is a result of the form `V v` (a regular value `v`) or `R v` (a value that was `return`ed).
- `<G,N,L>` are the name of the global, non-local and local variables that
are used in the scope where `e` occurs. We abbreviate it as `<S>` when
we don't need to use the elements.

We we `g:G` (resp `n:N` or `l:L`) to mean: `g : { x : '_a_x ∀ x ∈ G ..}`
and `s:S` to mean `(g:G, n:N, l:L)`.


We use the notation `s⊙x` (for ⊙ ∈ {Ⓖ, Ⓝ, Ⓛ}) to denote
the projection on field `x` of the `G`lobal, `N`on-local or `L`ocal record in `s`. Likewise, we write:
`{s with x⊙ = v}` to denote the update of the corresponding record with a new value `v` for field the `x`.

The straightforward encodings are the following. They are given by a function

```ocaml

⟦c⟧<s,S> =                      (* constant *)
    λs:S.(V c, s)

⟦e1; e2⟧<S> =                   (* sequence *)
     λs:S. match ⟦e1⟧ s with
          (R _, _) as r -> r
        | V v1, s1 -> ⟦e2⟧ s1      (* ← could warn if v1 is not () *)


⟦if e1 then e2 else e3⟧<S> =    (* conditional *)
     λs:S. match ⟦e1⟧ s with
        (R _, _) as r -> r
      | V v1, s1 -> if v1 then ⟦e2⟧ s1 else ⟦e3⟧ s1

⟦x⊙⟧<S> =                       (* variable read *)
     λs:S. (V(s⊙x), s)

⟦x⊙ = e⟧<S> =                   (* variable write *)
     λs:S. match ⟦e⟧ s with
          (R _, _) as r -> r
         | V v, s' -> V (), { s' with x⊙ = v}

⟦return e⟧<S> =                 (* return *)
     λs:S. match ⟦e⟧ s with
          (R _, _) as r -> r
         | V v, s' -> R v, s'

```
We can now present the two constructions, lambda and application
```ocaml
⟦λ<G,N,L>x.e⟧<Go,No,Lo> =       (* lambda *)
    (* Go, No, Lo, 'outer scope': the name of variables that
       are used in the scope where the lambda is defined *)

    λ(gs : Go, ns : No, ls : Lo). (* this is like s:S above *)
      (* gs, ns, ls are the global, nonlocal, local of the
         scope where f is defined *)
    let f =
        (* f is the closure that will be returned as the result
           of evaluating this expression.
           - f will receive the global scope in gs'
           - f will receive the local scope of the enclosing context
           - f cannot make any hypothesis on the non-local scope, except
           for its type, since we don't know from where f is called *)
         λx.λ(gs':G). (* see 1 below *)
           let ns'' = { x = ns.x ∀ x ∈ No ∩ N; x  = ls.x ∀ x ∈ Lo ∩ N } in
                      (* see 2 below *)
           let ls'' = { x = Undef ∀ x ∈ L } in (* see 3 below *)
           match ⟦e⟧ (gs', ns'', ls'') with
            (R _, _) as r -> r
          | (V _, s) -> (R None, s)
     in
     (V f, (gs, ns, ls))

⟦e1 e2⟧<S> =       (* application *)
    λs:S. match ⟦e1⟧ s with
         (R _, _) as r -> r
        | V v1, s1 -> match ⟦e2⟧ s1 with
                      (R _, _) as r -> r
                    | V v2, (g2, l2, s2) ->
                               match v1 v2 g2 with
                              | R v , (g3, _, _) -> V v (g3, l2, s2)
```
### Example
Consider the following:
```python
def f ():
  def g ():
    nonlocal a
    b = 42
    def h():
      nonlocal a,b
      global x
      c = a + b
      if c == 43:
        x = 'Foo'
      else:
        x = 56
    h()
  a = 1
  return g
u = f()
u()
print(x) # Foo
```
gets translated into mini-python
```python Ⓖ, Ⓝ, Ⓛ
fⒼ = λ<{},{},{g}>().
      gⓁ = λ<{},{a},{b,h}>().
              bⓁ = 42;
              hⓁ = λ<{x},{a,b},{c}>().
                      cⓁ = aⓃ + bⓃ;
                      if cⓁ == 43 then
                            xⒼ = "Foo"
                      else
                            xⒼ = 56      ;
              h()
      aⓃ = 1;
      return gⓁ()
uⒼ = fⒼ();
uⒼ()
```
this gives the encoding
```ocaml
(* term for the encoding of the lambda in h (the inner most) *)
λ (gs: { ..}, ns: {a : '_a_a; ..}, ls: { b : '_a_b; h : '_a_h .. })
λ().λ(gs':{X: '_a_x ..}).
  let ns'' = { a = ns.a; b = ls.b } in
  let ls'' = { c = Undef } in (* we use let's instead lambdas and remove some matches with R  *)
  let s = (gs', ns'', ls'') in
  let ra, s = V(sⓃa), s in     (* read a in ns'' *)
  let rb, s = V(sⓃb), s in     (* read a in ns'' *)
  let rc, s = V(()), { s with sⓁb = b+c} in     (* read a in ns'' *)
  let rc, s = V(sⓁc), s in




```









