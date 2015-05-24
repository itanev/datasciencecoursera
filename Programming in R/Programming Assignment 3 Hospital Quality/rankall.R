rankall <- function(outcome, num = "best") {
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	possible_outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
	
	data[, 11] <- as.numeric(data[, 11])
	data[, 17] <- as.numeric(data[, 17])
	data[, 23] <- as.numeric(data[, 23])
	
	states <- unique(data[, 7])
	
	if(!(outcome %in% names(possible_outcomes))) {
		stop("invalid outcome")
	}
	
	if(num != "best" && num != "worst" && !is.numeric(num)) {
		stop("Invalid num")
	}
	
	col_index <- possible_outcomes[outcome]
	
	data <- data[complete.cases(data[, col_index]),]
	
	transform <- function(state) {
		
		sub <- subset(data, State == state)
		
		ordered <- sub[order(sub[, col_index], sub[,2]),]

		if(num == "best") index <- 1
		else if(num == "worst") index <- length(ordered[, col_index]) 
		else index <- num
		
		hospitals <- rbind(hospitals, data.frame(hospital=ordered[index, 2], state=state))
	}
	
	results <- lapply(sort(states), transform)
	
	as.data.frame(do.call(rbind, results))
}
