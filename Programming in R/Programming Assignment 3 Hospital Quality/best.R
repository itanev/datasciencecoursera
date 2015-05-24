best <- function(state, outcome) {
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	possible_outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
	
	all_hospitals_from_state <- subset(data, State == state)
	
	if(!is.character(state) || nchar(state) <= 0 || nchar(state) > 2 || nrow(all_hospitals_from_state) == 0) {
		
		stop("invalid state")
	}

	if(!(outcome %in% names(possible_outcomes))) {
		stop("invalid outcome")
	}

	hospital_name <- NA

	data[, 11] <- as.numeric(data[, 11])
	data[, 17] <- as.numeric(data[, 17])
	data[, 23] <- as.numeric(data[, 23])

	col_index <- possible_outcomes[outcome]
	
	index <- which.min(all_hospitals_from_state[, col_index])
	hospital_name <- all_hospitals_from_state[index, 2]
}