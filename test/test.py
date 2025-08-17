def f(x, x):
    z = 46
    def g():
        nonlocal z
        global _
        x = 42
        z = z + u
    g()

    h = lambda u,v : (z:= v)

u = 50


class N:
    z = 10
    def __init__(self):
        self.u = 3

    

