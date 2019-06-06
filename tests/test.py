from module import SuperClass

# Basic test class

class LexerTestClassBasicKeywords(SuperClass):
	def __init__(self, *args, **kwargs):
		self.int_val = 0
		self.short_string_val = 'text - '
		self.long_string_val = """
			Long Text
			On Multiple Lines
		"""
		return super().__init__()

	def addition(self, a):
		return a + self.int_val

	def concat(self, text):
		return f'{text} {self.short_string_val}'

	def raise_exception(self):
		try:
			self.int_val + self.short_string_val
		except Exception:
			with open('path/to/file.txt', 'w') as fd:
				fd.write(self.long_string_val)
		assert type(self.int_val) == int

