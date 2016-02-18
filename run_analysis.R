## Coursera Getting and Cleaning Data Course Project

# run_analysis.R File Description:
# This script does the following

# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# creates a second, independent tidy data set with the average of each variable for each activity and each subject.

###################################################################################################################


#set working directory to the location where the UCI HAR Dataset was unzipped
setwd("C:/Users/RaoFamily/Documents/Malavika/DSc/gcd-wk4/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")

# Read in the data from files

features <- read.table('./features.txt',header=FALSE)
activityType <- read.table('./activity_labels.txt',header=FALSE)

subjectTrain <- read.table('./train/subject_train.txt',header=FALSE)
xTrain <- read.table('./train/x_train.txt',header=FALSE)
yTrain  <- read.table('./train/y_train.txt',header=FALSE)

subjectTest <- read.table('./test/subject_test.txt',header=FALSE)
xTest      <- read.table('./test/x_test.txt',header=FALSE)
yTest       <- read.table('./test/y_test.txt',header=FALSE)

# Assigin column names to the data imported above

colnames(activityType) <- c('activityId','activityType')

colnames(subjectTrain)  <- "subjectId"
colnames(xTrain)        <- features[,2] 
colnames(yTrain)        <- "activityId"

colnames(subjectTest) <- "subjectId"
colnames(xTest)       <- features[,2] 
colnames(yTest)       <- "activityId"

# create final train and test data
trainingData <- cbind(yTrain,subjectTrain,xTrain)
testData <- cbind(yTest,subjectTest,xTest)

# create final data by combining train and test data
finalData <- rbind(trainingData,testData)

library(dplyr)
alldata <- tbl_df(finalData)

# assigning appropriate variable names
names(alldata)<-gsub("std\\(\\)", "SD", names(alldata))
names(alldata)<-gsub("mean\\(\\)", "MEAN", names(alldata))
names(alldata)<-gsub("^t", "time", names(alldata))
names(alldata)<-gsub("^f", "frequency", names(alldata))
names(alldata)<-gsub("Acc", "Accelerometer", names(alldata))
names(alldata)<-gsub("Gyro", "Gyroscope", names(alldata))
names(alldata)<-gsub("Mag", "Magnitude", names(alldata))
names(alldata)<-gsub("BodyBody", "Body", names(alldata))

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & SD() columns

colNames <- colnames(alldata)
meansdcols1 <- grepl("activity|subject..|.*MEAN.*|.*SD.*", colNames)

alldata_meansd <- alldata[meansdcols1]

# creating separate dataset with average of each variable for each activity and each subject
tidydata <- aggregate(.~subjectId - activityId, data = alldata_meansd, mean)

# merging activity names with final tidy data
finaltidydata <- merge(tidydata,activityType,by="activityId",all.x=TRUE)

# export the tidy data set
write.table(finaltidydata, "TidyData.txt", row.names = FALSE)
