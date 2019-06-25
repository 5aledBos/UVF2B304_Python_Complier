try:
	x = 5
except ident1 as ident2  : 
	x = 7

try:
	x = 5
except ident1 as ident2:
	x = 7
except ident1 as ident2:
	x = 9
finally:
	x = 11

try:
	x = 5
except ident1 as ident2:
	x = 7
except ident1 as ident2:
	x = 9
else : 
	x = 11
finally:
	x = 13

try:
	x = openfile("path_to_file")
except Exception:
	x = closefile()


