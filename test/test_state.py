# def f (x,/):
    # if x:
      # y = 42
    # return (x,y)

# x = 2

# def f(z,/):
#     x = 2
#     def g(y,/):
#         nonlocal x
#         return x
#     return (g,5)

def f(x,/):
    fy = x
    def g(gx,/):
        gy = 2
        def h(hx,/):
          hy = 1
          return hx
        return fy
    return x

y = f(10)

# z = f("hey")

# def g(x,/):
#   global u
#   return (u,x)

# u = 43 #comment for error

# z = g(42)
