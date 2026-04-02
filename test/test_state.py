# def f (x,/):
    # if x:
      # y = 42
    # return (x,y)

# x = 2

def f(z,/):
    x = 2
    def g(y,/):
        nonlocal x
        return x
    return (g,5)

# y = f(10)

# def g(x,/):
#   global u
#   return (u,x)

# u = 43 #comment for error

# z = g(42)
