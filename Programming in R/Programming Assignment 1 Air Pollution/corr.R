corr <- function(directory, threshold=0) {
	
	result <- c()
	file_list <- list.files(directory)
	for(i in 1:length(file_list)) {
		current_file <- read.csv(paste(directory, file_list[i], sep="/"))
		cases <- complete.cases(current_file)
		complete_cases <- length(cases[cases == TRUE])
		
		if(complete_cases > threshold) {
			result <- c(result, cor(current_file[2], current_file[3],  use="pairwise.complete.obs"))
		}
	}

	result

}