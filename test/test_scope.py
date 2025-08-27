def f(x):                 #x parameter
    def g():              #g local
        nonlocal z        #z non local to g
        global _          #_ global
        x = 42            #x local to g
        z = z + u         #u global
    g()
    z = 46                #z local to f

    h = lambda u,v : (z:= v)  #h local to f, z local  to lambda, u,v parameters
    def j():                  #j local
        v = z                 #v local, z non local
        y = t                 #y local, t non local
    t = 42                    #t, local
    def k():
        return x              #x, nonlocal

u = 50                        #u global
f(1)

def g(x):                     #x parameter
    pass

class N:
    type ttt = 42
    z = 10                    #z local
    def __init__(self):       #self parameter, __init__ local (to N)
        self.u = 3            #self parameter

    def mumthoe(self : int):  #mumthoe local to N, self parmeter, int global
        pass

    def tt(self: ttt):
        pass

