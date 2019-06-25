async def function():
	return x

async with ident1 as ident2 :
	x =5

async with ident1 as ident2, ident as ident1:
	x = 5

async for ident in testlist :
	x = 5


async for i in range(1,10):
	x = 5
