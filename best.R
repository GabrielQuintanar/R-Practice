best <- function(state, outcome) {
      outcomes <- setOutcomes()
      if(!(outcome %in% names(outcomes)))
            stop("Invalid outcome")
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      if (!(state %in% data$State))
            stop("Invalid State")
      dataState <- data[data$State == state, ]
      dataStateOrd <- dataState[order(
            as.numeric(dataState[[outcomes[[outcome]]]]),
            dataState$Hospital.Name, na.last = NA), ]
      dataStateOrd$Hospital.Name[1]
}


setOutcomes <- function(){
      outcomes <- list("heart attack" = 11, "heart failure" = 17, 
                       "pneumonia" = 23)
      outcomes
}
