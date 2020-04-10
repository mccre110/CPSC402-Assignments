firstA = False
secondA = False
firstB = False
secondB = False
count = 1
location = []
file = input("Enter filename: ")

with open(file, "r") as f:
	while True:
		c = f.read(1)

		if c == 'a':
			if firstA == False and secondA == False and firstB == True:
				firstB = False
				firstA = True
			elif firstA == True and secondA == False:
				secondA = True
			elif firstA == False and secondA == False:
				firstA = True

		elif c == "b":
			if firstA == True and firstB == False:
				firstA = False
				secondA = False
				firstB = True
			elif firstB == True and secondB == False:
				secondB = True
			else:
				firstA = False
				secondA = False
				firstB = False
				secondB = False
				"""
				if first A is false, then dont care, leave secondB and firstB as false
				if firstB == True and second B == true
					then just make everything false, not looking for three B's in a row
				"""
		else:
			firstA = False
			secondA = False
			firstB = False
			secondB = False

		if secondB == True:
			firstB = False
			secondB = False
			location.append(count)
		if secondA == True:
			location.append(count)
		if not c:
			break

		count += 1

print(location)
