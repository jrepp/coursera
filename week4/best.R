 
#
# 
#

df = NULL

cached_reader <- function(...) {
    if (is.null(df)) {
        df <<- read.csv(...)
    } else {
      message("using cached result")
    }
    df
}

best <- function(state, outcome_name,reader=read.csv) {
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
  order_by_rate <- order(values, na.last=TRUE, decreasing=FALSE)
  outcomes_sorted <- statehospitals[order_by_rate,]
  
  # remove NAs 
  outcomes_sorted <- outcomes_sorted[!is.na(outcomes_sorted[outcome_name]),]
  
  # select all tied outcomes
  best_value <- values[order_by_rate][[1]]
  top_outcomes <- outcomes_sorted[outcomes_sorted[outcome_name] <= best_value,]
  
  # order alphabetically
  top_outcomes <- top_outcomes[order(top_outcomes$hospital),]
  
  # return the best value
  top_outcomes[1,1]
}


best("WA", "pneumonia", cached_reader)

