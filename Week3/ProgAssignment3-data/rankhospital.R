rankhospital <- function(state, outcomeName, num = "best") {
	## Read outcome data
	## Check that state and outcome are valid
	## Return hospital name in that state with the given rank
	## 30-day death rate
	
	outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	hospital <- read.csv("hospital-data.csv", colClasses = "character")
	outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
	
	filterByRank <- function(columnNumber, rank)
	{
		filteredData[, columnNumber] <- as.numeric(filteredData[, columnNumber])
		filteredData <- filteredData[!is.na(filteredData[, columnNumber]),]
		filteredData <- filteredData[order(filteredData[, columnNumber], filteredData[, 47]),]
		if(rank== "worst")
		{
			rank <- nrow(filteredData)
		}
			
		if(rank > nrow(filteredData))
		{
			NA
		}
		else
		{
			filteredData[rank, 47]
		}
	}
	
	filteredData <- outcome.hospital[outcome.hospital$State.y == state,]
 	
	if(num == "best")
	{
		num <- 1
	}
	
	if(nrow(filteredData) == "0")
	{
		stop("invalid state")
	}
	else
	{
		if(outcomeName == "heart attack")
		{
			filterByRank(11, num)
		}
		else if(outcomeName == "heart failure")
		{
			filterByRank(17, num)
		}
		else if(outcomeName == "pneumonia")
		{
			filterByRank(23, num)
		}
		else
		{
			stop("invalid outcome")
		}
	}
}
