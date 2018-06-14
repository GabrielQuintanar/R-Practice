rankhospital <- function(state, outcome, num) {
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
      num <- setNum(num, dataStateOrd)
      if(is.na(num))
            return(NA)
      dataStateOrd$Hospital.Name[num]
}

setOutcomes <- function(){
      outcomes <- list("heart attack" = 11, "heart failure" = 17, 
                       "pneumonia" = 23)
      outcomes
}

setNum <- function(num, data) {
      if(num == "best")
            num <- 1
      else if(num == "worst")
            num <- nrow(data)
      else if(!is.numeric(num))
            stop("No numeric value, or 'best'/'worst', for num")
      else if(num > nrow(data))
            return(NA)
      num
}