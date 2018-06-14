rankall <- function(outcome, num = "best") {
      outcomes <- setOutcomes()
      if(!(outcome %in% names(outcomes)))
            stop("Invalid outcome")
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      states <- unique(data$State)
      states <- states[order(states)]
      hospitalCol <- vector()
      stateCol <- vector()
      for (state in states) {
            dataState <- data[data$State == state, ]
            dataOrd <- dataState[order(
                  as.numeric(dataState[[outcomes[[outcome]]]]), 
                  dataState$Hospital.Name, na.last = NA), ]
            numSta <- setNum(num, dataOrd)
            if(is.na(numSta))
                  hospitalCol <- c(hospitalCol, NA)
            else
                  hospitalCol <- c(hospitalCol, dataOrd$Hospital.Name[numSta])
            stateCol <- c(stateCol, state)
      }
      data.frame(hospital = hospitalCol, state = stateCol)
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
            num <- NA
      num
}