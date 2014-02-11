printGist2 <- function() {
	outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	hospital <- read.csv("hospital-data.csv", colClasses = "character")
	outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")

	death <- as.numeric(outcome.hospital[, 11]) ## Heart attack outcome
	npatient <- as.numeric(outcome.hospital[, 15])
	owner <- factor(outcome.hospital$Hospital.Ownership)
	
	library(lattice)

	xyplot(death ~ npatient )
}