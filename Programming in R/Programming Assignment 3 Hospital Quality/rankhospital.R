rankhospital <- function(state, outcome, num = "best") {
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	possible_outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
	
	data[, 11] <- as.numeric(data[, 11])
	data[, 17] <- as.numeric(data[, 17])
	data[, 23] <- as.numeric(data[, 23])
	
	all_hospitals_from_state <- subset(data, State == state)
	
	if(!is.character(state) || nchar(state) <= 0 || nchar(state) > 2 || nrow(all_hospitals_from_state) == 0) {
		stop("invalid state")
	}

	if(!(outcome %in% names(possible_outcomes))) {
		stop("invalid outcome")
	}
	
	if(num != "best" && num != "worst" && !is.numeric(num)) {
		stop("Invalid num")
	}

	hospital_name <- NA

	col_index <- possible_outcomes[outcome]
	
	ordered <- all_hospitals_from_state[order(all_hospitals_from_state[, col_index], all_hospitals_from_state[,2]),]
	ordered <- ordered[complete.cases(ordered[, col_index]),]
	
	if(num == "best") index <- 1
	else if(num == "worst") index <- length(ordered[, col_index]) 
	else index <- num
	
	hospital_name <- ordered[index, 2]

}