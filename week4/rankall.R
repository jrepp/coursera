
rankstate <- function(statehospitals, outcome_name, top_n) {
  values <- statehospitals[[outcome_name]]
  
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

rankall <- function(outcome_name, top_n="best", reader=read.csv) {
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
  
  if (is.null(outcomes[1, outcome_name])) {
    stop("invalid outcome")
  }
  
  # split the outcome data by state
  bystate <- lapply(split(outcomes, states), rankstate, outcome_name, top_n)
  bystate <- stack(bystate)
  names(bystate) <- c("hospital", "state")
  bystate
}


