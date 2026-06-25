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

# def f(x,/):
#     fy = x
#     def g(gx,/):
#         gy = 2
#         def h(hx,/):
#           hy = 1
#           return hx
#         return fy
#     return x

# def id(x):
  # global u
  # if 2==0:
    # z = 1
  # def inner(aa): return aa
  # inner(0)
  # z=2
#    # def f(fx):
#        # fy = "totot"
#        # return
#    u = 42
  # return (x)
# u = 0
## y = 0
# y = id(10)
## z = id("hey")
## u="toto"


# def f(n):
#     u = n+1
#     z = f("toto")
#     return u / z

# g = f

# def f(b):
#     b = b and b
#     return 1-1

# g(42)

# id = lambda x:1
# def id(x):
  # y = 1
  # return x

y="hey"
def id(x):
  def ii(u):
    return x + u + y
  return ii(x+y)
y=0
z=id(3)
