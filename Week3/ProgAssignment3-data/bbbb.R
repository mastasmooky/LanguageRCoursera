printGist1 <- function() {
	outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcome[, 11] <- as.numeric(outcome[, 11])

	outcome2 <- outcome[table(outcome$State)[outcome$State] > 20, ]
	
	death <- outcome2[, 11]
	state <- outcome2$State
	boxplot(death ~ state)

}