# A Python program by Test McStudent, 
# Fall 2021



my_name = "Test McStudent"
my_year = 2023
print(f"{my_name}({my_year})")
print("CS150 FA2021")

def print_assignment(number, assignment_desc):
	text = f"A{number}: {assignment_desc}"
	# Print data about an assignment
	print(text)

assignments = ["Three IDES", "Student Data", "Dogs of NYC", "Tracery", "Bayesian Movies", "Matchmaking", "Webscraping", "Cats and Colors", "Three Languages"]

for i in range(0, 9):
	# Print each assignment
	desc = assignments[i]
	print_assignment(i, desc)


