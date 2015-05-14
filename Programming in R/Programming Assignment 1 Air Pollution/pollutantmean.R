pollutantmean <- function(directory, pollutant, id = 1:10) {
 	filenames <- sprintf("%03d.csv", id)
 	filenames <- paste(directory, filenames, sep="/")
 	ldf <- lapply(filenames, read.csv)
 	df=ldply(ldf)
 
 	mean(df[, pollutant], na.rm = TRUE)
 }