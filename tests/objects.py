class MyClass:

	def __init__(x, y):
		attr1 = x
		attr2 = y

	def getAtrr1():
		return attr1
	def setAttr1(x):
		attr1 = x

	def getAttr2():
		return attr2
	def setAttr2(y):
		attr2 = y

	def run():
		print("start run with attr1 = " + attr1)
		for i in attr2:
			if  i%2 == 0 :
				print(i*attr1)
		print("end run")

a = MyClass(1, [2,5,8,5,4,3,33,0,9])
b = MyClass(1, [2,5,8,5,4,3,33,0,9])

a.run()


a.setAttr1(7)

a.run()

b.run()
