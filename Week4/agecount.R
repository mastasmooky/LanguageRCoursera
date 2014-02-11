agecount <- function(age = NULL) {
	## Check that "age" is non-NULL; else throw error
	## Read "homicides.txt" data file
	## Extract ages of victims; ignore records where no age is
	## given
	## Return integer containing count of homicides for that age
	
	homicides <- readLines("homicides.txt")

	r <- regexec("", homicides)
	r <- regexec("((male|female), (.*?) years old)|(Age: (.*?) years old)", homicides)
		m <- regmatches(homicides, r)
		res <- sapply(m, function(x) sub("^\\s+", "", as.numeric(x[6])))

		filteredRes <- res[res == age]
		filteredRes <- filteredRes[!is.na(filteredRes)]
		firstCount <- length(filteredRes)

	r <- regexec("", homicides)
	r <- regexec("((male|female), (.*?) years old)|(Age: (.*?) years old)", homicides)
		m <- regmatches(homicides, r)
		res <- sapply(m, function(x) sub("^\\s+", "", as.numeric(x[4])))

		filteredRes <- res[res == age]
		filteredRes <- filteredRes[!is.na(filteredRes)]

	firstCount  + length(filteredRes)

	

}
