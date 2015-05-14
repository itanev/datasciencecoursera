complete <- function(directory, identifier=1:332) {
	
	file_list <- list.files(directory)
	data_frame <- data.frame()
	for(i in identifier) {
		current_file <- read.csv(paste(directory, file_list[i], sep="/"))
		cases <- complete.cases(current_file)
		data_frame <- rbind(data_frame, data.frame(id=i, nobs=length(cases[cases == TRUE])))
	}

	data_frame

}
