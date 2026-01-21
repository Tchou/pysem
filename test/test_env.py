U = 42
def f ():
    x = "toto"
    def u ():
        x = 3
        def h():
            nonlocal x
            global U
            y = x
            U = y
        def i():
            nonlocal y
            z = y
            global V
            V = U+z
        h()
        i()
    #Take only one of the y=...
    y = 42 # OK, V and U of type int
    # y = "foo" # type error doing addition U+z
    u()
    # y = 42 # Error, y is not yet defined when called.

f()
print(U, V) #if f suceeds, U and V are defined to 3 and 45