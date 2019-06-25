from module import SuperClass

class LexerTestClassBasicKeywords(SuperClass):
	def __init__(self, int_val, string_val):
		self.int_val = int_val
		self.short_string_val = string_val

	def addition(self, a):
		return a + self.int_val

	def concat(self, text1, text2):
		return text1 + text2



