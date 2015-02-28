 
#
# 
#

df = NULL

cached_reader <- function(...) {
    if (is.null(df)) {
        df = read.csv(...)
    }
    return df
}



best <- function(state, outcome_name,reader=read.csv) {
  # Use the specified reader to get the data frame
  outcomes <- reader("outcome-of-care-measures.csv", colClasses= "character")
  colnames <- names(outcome)
  states <- factor(outcome$State)
  # columns
  # 2 - hospital name
  # 11 - heart attack 30 day mortality
  # 17 - heart failure
  # 23 - pneumonia
  outcomes = outcomes[,c(2, 11, 17, 23)]
  names(outcomes) <- c("hospital", "heart attack", "heart failure", "pneumonia")
  
  # split the outcome data by state
  bystate <- split(outcomes, states)
  
  # select just the state hospitals that we care about
  statehospitals <- bystate[[state]]
  if(statehospitals == NULL) {
    stop("invalid state")
  }
  values = statehospitals[[outcome_name]]
  if (values == NULL) {
    stop("invalid outcome")
  }
  # sort the hospitals in increasing order by outcome
  sorted_by_rate = order( decreasing=TRUE)
  names = head(statehospitals[sorted_by_rate,])
}


best("WA", "", cached_reader)

