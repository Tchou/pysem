def f(x):
    z = 46
    def g():
        nonlocal z, z
        global _
        x = 42
        z = z + u
    g()

    h = lambda u,v : (z:= v)
u = 50
f(1)

def g(x; ):
    pass

class N:
    z = 10
    def __init__(self):
        self.u = 3

    def mumthoe(self : int):
        pass


