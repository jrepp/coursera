corr <- function(directory, threshold = 0) {
	correlations = c()

	## process all observerations above the threshold
	for (i in 1:332) {
		## read in the observations
		
		file = file.path(directory, paste(sprintf("%03d", i), ".csv", sep=""))
		## print(file)
		## print(class(file))
		df = read.csv(file)
		
		## select for complete samples
		samples = df[complete.cases(df),]

		if (length(samples[[1]]) > threshold) {
			nitrate = samples$nitrate
			sulfate = samples$sulfate
			correlations = c(correlations, cor(nitrate, sulfate))
			## print(paste("processing ", file))
		}
	}

	correlations
}

## corr("specdata", 16)