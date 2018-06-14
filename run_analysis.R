#Download zip file and unzip it to begin with extraction
#Set a new Working Directory
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "wearableData.zip")
unzip(zipfile = "WearableData.zip")
setwd("./UCI HAR Dataset")

#1. Merges the training and the test sets to ceate one data set
#Read all necessary text files
      
xTrainData <- read.table("./train/X_train.txt")
xTestData <- read.table("./test/X_test.txt")

yTrainData <- read.table("./train/Y_train.txt")
yTestData <- read.table("./test/Y_test.txt")

subjectTrain <- read.table("./train/subject_train.txt")
subjectTest <- read.table("./test/subject_test.txt") 

features <- read.table("features.txt")

activityLabels <- read.table("activity_labels.txt")

#It is necessary to assign names for columns, as a good practice for tidy data
#Features info mention how is settled features info in features_info.txt. So
#Column 2 has all column names for X Train text file and Y Train text file.
#Lastly, either X and Y Test files just need to assign 'activity_id' as a column name.
colnames(xTrainData) <- features[ , 2]
colnames(xTestData) <- features[ , 2]

colnames(yTrainData) <- "activity_id"
colnames(yTestData) <- "activity_id"

#After reading carefully Readme file, and failing some times, it was better to assign also
#a column name for subject files
colnames(subjectTrain) <- "subject_id"
colnames(subjectTest) <- "subject_id"

colnames(activityLabels) <- c("activity_id", "type")

#Load libraries learnt in this course for merge all the data
library(tidyr);library(plyr); library(dplyr)

trainData <- bind_cols(xTrainData, yTrainData, subjectTrain)

testData <- bind_cols(xTestData, yTestData, subjectTest)

merged <- bind_rows(trainData, testData)



# 2. Extracts only the measyrements on the mean and standard deviation for each measurement
mergedTbl <- tbl_df(merged)
meanData <- select(mergedTbl, grep("mean", names(mergedTbl)))
stdData <- select(mergedTbl, grep("std", names(mergedTbl)))
ids <- select(mergedTbl, activity_id, subject_id)
meanStdData <- bind_cols(meanData, stdData, ids)

# 3. Uses descriptive activity names to name the activities in the dataset
meanStdDataLabeled <- merge(meanStdData, activityLabels, by = 'activity_id', all.x = TRUE)

# 4. Appropriately labels the data set with descriptive variable names
names(meanStdDataLabeled) <- tolower(names(meanStdDataLabeled))
names(meanStdDataLabeled) <- gsub("-", "", names(meanStdDataLabeled))
names(meanStdDataLabeled) <- gsub("_", "", names(meanStdDataLabeled))
names(meanStdDataLabeled) <- gsub("acc", "acceleration", names(meanStdDataLabeled))
names(meanStdDataLabeled) <- gsub("std", "standarddeviation", names(meanStdDataLabeled))
names(meanStdDataLabeled) <- gsub("mag", "magnitude", names(meanStdDataLabeled))
names(meanStdDataLabeled) <- gsub("^t", "time", names(meanStdDataLabeled))
names(meanStdDataLabeled) <- gsub("^f", "fourier", names(meanStdDataLabeled))

# 5. From the data set in step 4, creates a second, independent tidy data set with the
# average of each variable for each activity and each subject

tidySet <- aggregate(. ~ subjectid + activityid, meanStdDataLabeled, mean)

#Order by subject id and activity id
tidySet <- arrange(tidySet, subjectid, activityid)

write.csv(tidySet, file = "tidySet.txt", row.names = FALSE)
