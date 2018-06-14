pollutantmean <- function(directory, pollutant, id = 1:332) {
      directoryC <- paste(directory, "/", sep="")
      files <- list.files(path = directoryC)
      total = 0
      noRows = 0
      for (v in id) {
            sum = 0
            fileRead <- read.csv(
                  paste(directoryC, files[v], sep = "")
                  , header = T, sep = ",")
            fileFiltered <- fileRead[
                  complete.cases(fileRead[pollutant]), ]
            noRows = noRows + nrow(fileFiltered)
            for (pol in 1:nrow(fileFiltered)) {
                        sum = sum + fileFiltered[pol, ][pollutant]
            }
            total <- total + sum
            }
      total <- total / noRows
}

complete <- function(directory, id = 1:332){
      directoryC <- paste(directory, "/", sep = "")
      files <- list.files(path = directoryC)
      nameFiles <- files[id]
      rowsComplete <- vector(mode = "list", length = length(id))
      y = 1
      for (x in id) {
            fileRead <- read.csv(
                  paste(directoryC, files[x], sep = ""),
                        header = T, sep = ","
            )
            rowsTotal <- sum(complete.cases(fileRead))
            rowsComplete[y] <- rowsTotal
            y = y+1
      }
      nameFiles <- parseInteger(nameFiles, ".csv")
      my_matrix <- cbind(nameFiles, rowsComplete)
      colnames(my_matrix) <- c("id", "nobs")
      my_matrix
}

complete2 <- function(directory, id = 1:332){
      summsData <- function(x) {
            data <- read.csv(paste(directory, "/", 
                                   formatC(x, width = 3, flag = "0"),
                                   ".csv", sep = ""))
            sum(complete.cases(data))
      }
      nobs <- sapply(id, summsData)
      return(data.frame(id, nobs))
}


parseInteger <- function(v, string) {
      for (x in 1:length(v)) {
            v[x] <- as.numeric(strsplit(v, string)[[x]])
      }
      v
}

corr <- function(directory, treshold = 0) {
      directoryC <- paste(directory, "/", sep = "")
      framed <- getCompleteCases(directoryC)
      idsT <- framed[framed["nobs"] > treshold, ]$id
      dataLast <- numeric()
      for (x in idsT) {
            dataRaw <- read.csv(paste(directoryC, 
                                      formatC(x, width = 3, flag = "0"),
                                      ".csv", sep = ""))
            framed2 <- dataRaw[complete.cases(dataRaw), ]
            resultCor <- cor(framed2$sulfate, framed2$nitrate)
            dataLast <- c(dataLast, resultCor)
      }
      dataLast
}

getCompleteCases <- function(fileDirectory, id = 1:332) {
      summsData <- function(x) {
            data <- read.csv(paste(fileDirectory, 
                                   formatC(x, width = 3, flag = "0"),
                                   ".csv", sep = ""))
            sum(complete.cases(data))
      }
      nobs <- sapply(id, summsData)
      return(data.frame(id, nobs))
}



pollutantmean2 <- function(directory, pollutant, id = 1:332) {
      
      setwd(file.path(getwd(), directory)) ## setting the directory
      total = 0                            ## the sum of all observed values of pollutant (either sulfate or nitrate)
      observations = 0                     ## the total number of observed values of pollutant (either sulfate or nitrate)
      
      #Looping thru the directory's files specified in the 'id' argument 
      for (i in id)
      {
            
            
            ## Due to the format of the filename, i.e 001, 010  instead of 1, 10. I became aware that the following method works but not efficient, 
            ## but at the time of the completion of this assignment, it was the only way I knew how to do it.           
            if (i <10) { 
                  data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
                                   header = T, 
                                   na.strings=c("NA","NaN", " "))
            }
            
            else if (i>=10 & i<100) { 
                  data <- read.csv(paste("0", as.character(i), ".csv", sep=""),  ## for example, if 'id' = 17, we get 017.csv
                                   header = T, 
                                   na.strings=c("NA","NaN", " ") 
                  )
            }
            
            
            
            else       { 
                  data <- read.csv(paste(as.character(i), ".csv", sep=""),     ## Normal
                                   header = T, 
                                   na.strings=c("NA","NaN", " ") 
                  )
            }
            
            ## getting rid of all the "NA" values and, consequently, all the non-complete ovservations (the ones with at least one NA in row)
            data = na.omit(data)    
            ##  cumulative addition of the complete observations
            observations = observations + nrow(data)
            ## depending the poluttant ( sulfate or nitrate), we aggregate the observed values
            if (pollutant == "sulfate") {total = total + sum(data$sulfate)}
            else {total = total + sum(data$nitrate)}
            
      }
      
      ## reset directory path
      setwd("..")
      ## returning the mean of the pollutant values
      return (total/observations)
      
}