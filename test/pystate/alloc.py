def alloc(x):
  def s(v):
    nonlocal x
    x = v
  def g(t):
    return x
  return (s)

r1 = alloc(40)
# r2 = alloc("hey")
r1(5)
