#Regular function
def f1(a,b,c,/,d,e,f,*,g,h,i):
    return (a,b,c,d,e,f,g,h,i)

#Application
f1 (1,2,3,d=4,e=5,f=6,i=7,h=8,g=9)

#Higher order function, the type of h is approximated as
# ((u=42) -> 'I47) & ((17) -> 'I46))
def f2(h):
    return h(17), h(u=42)

#All the following candidates type-check

def h0(u):
    return u+1

f2(h0)

def h1(a = 0, /, u=10):
    return a + u

f2 (h1)

def h3(u=10):
    return u+1

f2 (h3)

def h4 (u, b=42):
    return u

f2 (h4)

#The following doesn't

def bad(u, v):
    return u + v

#f2 (bad)  # does not type check, which is ok, bad has two mandatory arguments


def f3():
    return lambda x=42, y=45: x + y

f3()

def f4(g):
    return (g(1,2,z=4),g(u=1, v=2))

def g0(u=1,v=2,z=3):
    return u+v+z

f4(g0)


def g1(x=1, y=2, /, u=1, *,v=2,z=3):
    return x+y+u+v+z

f4(g1)

def g2(*,u=1,v=2,z=3):
    return u+v+z

#f4(g2)  #type error, the call g(1,2,z=4) in f4 is not well typed

