best <- function(state, outcome) {
      outcomes <- list("heart attack" = 11, "heart failure" = 17, 
                       "pneumonia" = 23)
      if(!(outcome %in% names(outcomes)))
            return("Outcome parameter is not valid")
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      if (!(state %in% data$State))
            return("State parameter is not valid")
      dataState <- data[data$State == state, ]
      dataStateOrd <- dataState[order(dataState[[outcomes[[outcome]]]]), ]
      dataStateOrd$Hospital.Name[1]
}