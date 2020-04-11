first = False
second = False 
third = False
four = False
five = False
six = False
count = 1
location = []
file = input("Enter filename :")
with open(file, "r") as f:
	while True:
		c = f.read(1)

		if c == 'a':
			if first == False:
				first = True
			elif (five == True and six == False):
				six = True
			elif (first == True and second ==True and third == True):
				four = False
				five = False
				six = False
			else:
				first = False
				second = False
				third = False
				four = False
				five = False
				six = False

		elif c == 'b':
			if first == True and second == False:
				second = True
			elif (four ==True and five == False):
				five = True
			elif (first == True and second ==True and third == True):
				four = False
				five = False
				six = False
			else:
				first = False
				second = False
				third = False
				four = False
				five = False
				six = False
		elif c == 'c':
			if (second == True and third == False):
				third = True
			elif (third == True and four == False):
				four = True
			elif(first == True and second ==True and third == True):
				five = False
				six = False
			else:
				first = False
				second = False
				third = False
				four = False
				five = False
				six = False

		elif (first == True and second ==True and third == True):
			four = False
			five = False
			six = False
		else:
			first = False
			second = False
			third = False
			four = False
			five = False
			six = False

		if six == True: 
			four = False
			five = False
			six = False
			location.append(count)
		if not c:
			break
		count += 1
print(location)
