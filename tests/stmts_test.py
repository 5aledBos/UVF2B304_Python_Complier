def return1():
	return 1

def return2():
	return 2

def return3():
	return return1() + return2()

print(return3())

def if_stmt(a, b):
	if a == b :
		return 0
	else :
		return 1

print(if_stmt(True,True))
print(if_stmt(False, True))


for i in [0, 1, 2, 3, 5] : 
	print(i**2)

for i in range(5):
	print(i**i)
