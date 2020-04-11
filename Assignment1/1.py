first = False
second = False 
third = False
last = False
count = 1
location = []
file = input("Enter filename :")
with open(file, "r") as f:
	while True:
		c = f.read(1)

		if c == 'a':
			if first == False:
				first = True
			elif second == True and third == False:
				third = True
			else:
				second = False
				third = False

		elif c == "b":
			if first == True and second == False:
				second = True

			elif third == True and last == False:
				last = True

			else:
				first = False
				second = False

		else:
			first = False
			second = False
			third = False
			last = False
			
		if last == True: 
			third = False
			last = False
			location.append(count)
		if not c:
			break
		count += 1
print (location)
