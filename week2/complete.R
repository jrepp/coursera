complete <- function(directory, file.ids=1:332) {
  nobs = c()
  id = c()
  for (i in file.ids) {
    ## read in the observations
    file = file.path(directory, paste(sprintf("%03d", i), ".csv", sep=""))
    df = read.csv(file)
    complete = df[complete.cases(df),]
    nobs = c(nobs, length(complete[[1]]))
    id = c(id, i)
  }
  
  data.frame(id, nobs)
}