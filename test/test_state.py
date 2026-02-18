def f (x,/):
  return x
y = f(10)

def g(x,/):
  global u
  return (u,x)
u = 43 #comment for error
z = g(42)