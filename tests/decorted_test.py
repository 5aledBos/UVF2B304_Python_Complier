@ident1(ident2)
def function():
	return x

@ident1(*ident2)  
def function():
	return x

@ident1(**ident2)
def function():
	return x

@ident1(ident2=ident3)
def function():
	return x

@ident1(ident2 async for ident3 in identlist)
def function():
	return x

@ident1(ident2 async for ident3 in identlist if lambda *args : ident4)
def function():
	return x

@ident1.ident2()
def function():
	return x
