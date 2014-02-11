rankall <- function(outcomeName, num = "best") {
	## Read outcome data
	## Check that state and outcome are valid
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name

	outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	hospital <- read.csv("hospital-data.csv", colClasses = "character")
	outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")

	if(num == "best")
	{
		num <- 1
	}

	filterByRank <- function(data, columnNumber, rank)
	{
		data [, columnNumber] <- as.numeric(data[, columnNumber])
		data <- data[!is.na(data[, columnNumber]),]
		data <- data[order(data[, columnNumber], data[, 47]),]
		if(rank== "worst")
		{
			rank <- nrow(data)
		}
			
		if(rank > nrow(data))
		{
			NA
		}
		else
		{
			data[rank, 47]
		}
	}

	states <- c()
	hospitals <- c()
	 
	for (state in names(table(outcome.hospital$State.y)) )
	{
		filteredByState <- outcome.hospital[outcome.hospital$State.y == state,]
		if(outcomeName == "heart attack")
		{
			hospitals <- c(hospitals,filterByRank(filteredByState, 11, num))
		}
		else if(outcomeName == "heart failure")
		{
			hospitals <- c(hospitals, filterByRank(filteredByState, 17, num))
		}
		else if(outcomeName == "pneumonia")
		{
			hospitals <- c(hospitals, filterByRank(filteredByState, 23, num))
		}
		states <- c(states, state)
 	}	

	data.frame(hospital = hospitals, state = states)
}
