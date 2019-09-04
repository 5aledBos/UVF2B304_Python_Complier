class MyClass2:

    def __init__(x,y):
	i = x
	j = y

    def getI():
        return i


    def setI(v):
	i = v

    def getJ():
	return j

    def setJ(y):
	j = y
	

def compare_objects(obj1, obj2):
	if  obj1.getI() != obj2.getI() :
		return False
	if obj1.getJ() != obj2.getJ() :
		return False
	return True


a = MyClass2(0, 0)
b = MyClass2(0, 0)
print(compare_objects(a,b))



a.setI(5)

print(compare_objects(a,b))

