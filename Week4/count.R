count <- function(cause = NULL) {
	## Check that "cause" is non-NULL; else throw error
	## Check that specific "cause" is allowed; else throw error
	## Read "homicides.txt" data file
	## Extract causes of death
	## Return integer containing count of homicides for that cause
	homicides <- readLines("homicides.txt")

	if(cause == "asphyxiation" | cause == "blunt force" |  
		cause == "other" | cause == "shooting" | 
		cause == "stabbing" | cause == "unknown")
	{
		r <- regexec("<dd>Cause:(.*?)</dd>", homicides)
		m <- regmatches(homicides, r)
		res <- sapply(m, function(x) sub("^\\s+", "", tolower(x[2])))

		filteredRes <- res[res == cause]
		filteredRes <- filteredRes[!is.na(filteredRes)]
		length(filteredRes)
	}
	else
	{
		stop("Wrong cause")
	}
}