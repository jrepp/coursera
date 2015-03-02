
rankhospital <- function(state, outcome_name, top_n, reader=read.csv) {
  # Use the specified reader to get the data frame
  outcomes <- reader("outcome-of-care-measures.csv", colClasses= "character")
  states <- factor(outcomes$State)
  
  # columns
  # 2 - hospital name
  # 11 - heart attack 30 day mortality
  # 17 - heart failure
  # 23 - pneumonia
  outcomes = outcomes[,c(2, 11, 17, 23)]
  names(outcomes) <- c("hospital", "heart attack", "heart failure", "pneumonia")
  outcomes[[2]] <- suppressWarnings(as.numeric(outcomes[[2]]))
  outcomes[[3]] <- suppressWarnings(as.numeric(outcomes[[3]]))
  outcomes[[4]] <- suppressWarnings(as.numeric(outcomes[[4]]))
  
  # split the outcome data by state
  bystate <- split(outcomes, states)
  
  # select just the state hospitals that we care about
  statehospitals <- bystate[[state]]
  if(is.null(statehospitals)) {
    stop("invalid state")
  }
  
  values <- statehospitals[[outcome_name]]
  if (is.null(values)) {
    stop("invalid outcome")
  }
  
  # sort the hospitals in increasing order by outcome
  order_by_rate <- order(values, statehospitals$hospital, na.last=TRUE, decreasing=FALSE)
  outcomes_sorted <- statehospitals[order_by_rate,]
  
  # remove NAs
  outcomes_sorted <- outcomes_sorted[!is.na(outcomes_sorted[outcome_name]),]
  
  if (top_n == "best") top_n <- 1
  else if (top_n == "worst") top_n <- length(outcomes_sorted[[1]])
  else if (top_n > length(outcomes_sorted[[1]])) return(NA)
  
  # return the rank value
  outcomes_sorted[top_n,1]
}


