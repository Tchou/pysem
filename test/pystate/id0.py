def f(y):
  # nonlocal z
  # z = 42
  return y

def id(x):
  # def f(y):
    # nonlocal z
    # z = 42
    # return y
  # def g(gy):
    # nonlocal z
    # z = "hello"
  z = 0
  f(0)
  # g(0)
  return z

#def id2(x):
#  return x
#def id3(x):
#  return x
#def id4(x):
#  return x
#def id5(x):
#  return x
#def id6(x):
#  return x
