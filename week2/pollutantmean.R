pollutantmean <- function(directory, pollutant, id = 1:332) {
	total_samples = c()
	for (i in id) {
		## read in the observations
		file = file.path(directory, paste(sprintf("%03d", i), ".csv", sep=""))
		df = read.csv(file)

		## filter the NA values and select samples
		samples = df[!is.na(df[[pollutant]]),][[pollutant]]

		## concat samples
		total_samples = c(total_samples, samples)
	}
	mean(total_samples)
}