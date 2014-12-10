def alana(b,c):
	if (b < 0):
		return True
	if (c < 6):
		return False
	return True

def teste(a, b):
	soma = 0
	for i in range(3):
		soma = soma + a + b
	print(soma)

def recebe(a):
	a = "teste"
	print (a)

x = alana(3,8)

if (x == True):
	print("True")
else:
	print("False")

x = 0

teste (x, 5)

a = "alana"

recebe(a)
