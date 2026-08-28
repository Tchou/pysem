def id(y):
  # nonlocal z
  # z = 42
  return y

def f(x):
  # def f(y):
    # nonlocal z
    # z = 42
    # return y
  # def g(gy):
    # nonlocal z
    # z = "hello"
  z = 0
  id(0)
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
