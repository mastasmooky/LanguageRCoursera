corr <- function(directory, threshold = 0) {
        ids <- 1:332
	  nobses <- numeric(0)
	  for (concreateId in ids)
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

		 if(NROW(na.omit(monitorData)) > threshold)
		 {
			nobses <- c(nobses, cor(na.omit(monitorData)$sulfate,na.omit(monitorData)$nitrate))
		 }
	  }
	  
        nobses

}