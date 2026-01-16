# TEST FUN_SPEC WITHOUT DEFAULT
#0
def f_0(): pass
#1
def f_1(x,/): pass
def f_2(y): pass
def f_3(*v): pass
def f_4(*,k): pass
def f_5(**a): pass

#2(1)
def f_6(x,/,y): pass
def f_7(x,/,*v): pass
def f_8(x,/,*,k): pass
def f_9(x,/,**a): pass
#2(2)
def f_10(y,*v): pass
def f_11(y,*,k): pass
def f_12(y,**a): pass
#2(3)
def f_13(*v,k): pass
def f_14(*v,**a): pass
#2(4)
def f_15(*,k,**a): pass

#3(1)
def f_16(x,/,y,*v): pass
def f_17(x,/,y,*,k): pass
def f_18(x,/,y,**a): pass
def f_19(x,/,*v,k): pass
def f_20(x,/,*v,**a): pass
def f_21(x,/,*,k,**a): pass
#3(2)
def f_22(y,*v,k): pass
def f_23(y,*v,**a): pass
def f_24(y,*,k,**a): pass
#3(3)
def f_25(*v,k,**a): pass

#4
def f_26(x,/,y,*v,k): pass
def f_27(x,/,y,*v,**a): pass
def f_28(x,/,y,*,k,**a): pass
def f_29(x,/,*v,k,**a): pass
def f_30(y,*v,k,**a): pass

#5
def f_31(x,/,y,*v,k,**a): pass


## TEST DEFAULTS
def f (a=1, b=2, /,c=3): pass

def g (a, b, /, c, d=1, *, e=3, f=4 ): pass

def h (a, b, c, d): pass

def i (a=1, b=2, /, c=3, d=4, e=5): pass

