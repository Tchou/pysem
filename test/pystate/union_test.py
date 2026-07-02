def f(x):
  if x and x: # => x is True
    if x:
      return x
    else: # should not be typed
      return 2
  else: # => x is ~True
    return "string"

y = f(True)
z = f(False)
