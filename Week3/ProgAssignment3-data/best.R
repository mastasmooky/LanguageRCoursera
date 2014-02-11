best <- function(state, outcomeName) {
	## Read outcome data
	## Check that state and outcome are valid
	## Return hospital name in that state with lowest 30-day death
	## rate

	outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	hospital <- read.csv("hospital-data.csv", colClasses = "character")
	outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
	
	filteredData <- outcome.hospital[outcome.hospital$State.y == state,]
	if(nrow(filteredData) == "0")
	{
		stop("invalid state")
	}
	else
	{
		if(outcomeName == "heart attack")
		{
			filteredData[, 11] <- as.numeric(filteredData[, 11])
			bestHospital <- filteredData[which.min(filteredData[,11]),]
			bestHospital[, 47]
		}
		else if(outcomeName == "heart failure")
		{
			filteredData[, 17] <- as.numeric(filteredData[, 17])
			bestHospital <- filteredData[which.min(filteredData[,17]),]
			bestHospital[, 47]
		}
		else if(outcomeName == "pneumonia")
		{
			filteredData[, 23] <- as.numeric(filteredData[, 23])
			bestHospital <- filteredData[which.min(filteredData[,23]),]
			bestHospital[, 47]
		}
		else
		{
			stop("invalid outcome")
		}
	}
}
