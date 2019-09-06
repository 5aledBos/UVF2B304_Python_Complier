class class1:

	def __init__():
		attr = 0

	def getAttr():
		return attr
	def increment_i():
		for j in range(10):
			attr =  getAttr() + 1

a = class1()

myList = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

print("value of attr : " + a.getAttr())


print("incrementing attr")
for i in myList:
	a.increment_i()

print("value of attr : " + a.getAttr())

print("length of myList : " + len(myList))


string = "this is a string"

print(len(string))

i = 5


print("should raisse error ==> ")
print(len(i))
