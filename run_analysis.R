require(dplyr)
require(tidyverse)
require(reshape2)
require(tidyr)

## dowloading the zipped file from the web
## creating a directory if none exists
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("./data")) {dir.create("./data")}
download.file(fileUrl, destfile = "./data/data.zip")
outDir <- "./data/unzipfolder"
unzip("./data/data.zip", exdir = outDir)

## reading the data into tables
## activity labels in table form
labels <- read.table("./data/unzipfolder/UCI HAR Dataset/activity_labels.txt")
features <- read.table("./data/unzipfolder/UCI HAR Dataset/features.txt")

## values of training data (x_train), IDs (y_train), and
##      subject# (subject) in a table
x_train <- read.table("./data/unzipfolder/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/unzipfolder/UCI HAR Dataset/train/y_train.txt")
subject <- read.table("./data/unzipfolder/UCI HAR Dataset/train/subject_train.txt")
## values of training data (x_test), IDs (y_test), and
##      subject# (subject_test) in a table
x_test <- read.table("./data/unzipfolder/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/unzipfolder/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/unzipfolder/UCI HAR Dataset/test/subject_test.txt")

## adding the y_train and subject# to new tibble (trainingSet) from x_train
trainingSet <- as_tibble(x_train) %>% rowid_to_column()
trainingSet$rowid <- y_train[,1]
trainingSet <- cbind(subject, trainingSet)

## adding the y_test and subject# to new tibble (testSet) from x_test
testSet <- as_tibble(x_test) %>% rowid_to_column()
testSet$rowid <- y_test[,1]
testSet <- cbind(subject_test, testSet)

## Part 4: setting the variable names of the columns of each data set
featurelabel <- as.character(featurelabel)
featurelabel <- gsub('-mean', 'mean', featurelabel)
featurelabel <- gsub('-std', 'std', featurelabel)
featurelabel <- gsub('[-()]', '', featurelabel)
colnames(trainingSet) <- c("subject", "activity",
                           featurelabel)
colnames(testSet) <- c("subject", "activity",
                       featurelabel)

## PART 1: Merging the test data and the training data
allData <- rbind(testSet, trainingSet)
## PART 3: setting descriptive activity labels and subject labels
allData$activity <- factor(allData$activity, levels = labels[,1], 
                                labels = labels[,2])
allData$subject <- as.factor(allData$subject)


## PART 2: Extracts only the measurements on the mean 
##      and standard deviation for each measurement.
meanstd_allData <- allData[, c(1,2,grep(".*mean.*|.*std.*", 
                                        colnames(allData)))]

## PART 5: From the data set in step 4, creates a second, independent tidy 
##      data set with the average of each variable for each activity and 
##      each subject.
meltalldata <- melt(meanstd_allData, id = c("subject", "activity"))

## using dcast function casting mean on subject and activity by variable.
dcastdata <- dcast(meltalldata, subject + activity ~ variable, mean)

## writing the tidied data to an output file called combinedData.txt
outfile <- "./data/combinedData.txt"
write.table(dcastdata, file = outfile, sep = " ", col.names = TRUE )