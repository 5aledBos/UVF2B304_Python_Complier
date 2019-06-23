global ident, ident1
nonlocal ident, ident1

del ident[ident]

del ident

pass


assert ident, ident1

assert ident or ident1, ident

assert ident and ident1, ident or ident1

#this is a comment

a = "this is a string"

# flow statements 

# break

for i in range(1,10):
	break

# return 

def function():
	return true

# continue

for i in list1:
	if a == 5 :
		continue

# raise

if a == 5:
	raise exception from Exception

# yield

yield

yield from ident

yield test, ident2


