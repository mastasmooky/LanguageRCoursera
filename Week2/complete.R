complete <- function(directory, id = 1:332) {
       
        ids <- c()
	  nobses <- c()
	  for (concreateId in id)
	  {
		changedId <- concreateId 
   		idLength <- nchar(concreateId)
	  	if(idLength == 1)
	  	{
			changedId <- paste("00", concreateId, sep='')
	  	}
	  	else if(idLength == 2)
	  	{
			changedId <- paste("0", concreateId, sep='')
	  	}

        	fileName <- paste(directory, "/", changedId , ".csv", sep='')
        	monitorData <- read.csv(file = fileName, header = TRUE)

		ids <- c(ids, concreateId)
		nobses <- c(nobses, NROW(na.omit(monitorData)))
	  }
	 
        data.frame(id = ids, nobs = nobses) 
}