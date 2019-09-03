class MyClass2:

    i = 0
	j = 0


    def getI():
        return i

    def setI(z):
        i = z

	


a = MyClass2()



b = MyClass2()

def compare(x,y):
	if x == y :
		print("hello")
	else :
		print("not")


compare(a.getI(), b.getI())

a.setI(7)

compare(a.getI(), b.getI())
