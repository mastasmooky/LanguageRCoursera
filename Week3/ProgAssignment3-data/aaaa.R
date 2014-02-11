printGist <- function() {
	outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      outcome[, 11] <- as.numeric(outcome[, 11])
	outcome[, 17] <- as.numeric(outcome[, 17])
      outcome[, 23] <- as.numeric(outcome[, 23])
      par(mfrow = c(3, 1)) 
      hist(outcome[, 11], xlab="30-day Death Rate", main="Heart attack", xlim=c(5,20), prob=TRUE, density=5) 
	hist(outcome[, 17], xlab="30-day Death Rate", main="Heart failure", xlim=c(5,20), prob=TRUE, density=5)
      hist(outcome[, 23], xlab="30-day Death Rate", main="Pneumonia", xlim=c(5,20), prob=TRUE, density=5)
}