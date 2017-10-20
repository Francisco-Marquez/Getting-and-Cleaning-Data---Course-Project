
#----------------------------- Final Project - Getting and Cleaning Data Course -----------------------------#
# Author: Francisco Marquez M.

# The R script have to do:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Downloading the data set:
# - Set the working directory, download the .zip data set, then unzip it.
path = getwd()
fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./dataProject.zip", method="wininet")
unzip(zipfile="dataProject.zip")

# - Load and clean activity_labels and features (.txt).
# - featuresObj: select measures names with mean and standard deviation (grep function) and clean it (gsub function).
activityLabels = read.table(file.path(path,"UCI HAR Dataset/activity_labels.txt"), col.names=c("classLabels","activityName"))
features = read.table(file.path(path,"UCI HAR Dataset/features.txt"), col.names=c("index","featureNames"))
features$featureNames = as.character(features$featureNames)
positions = grep("(mean|std)\\(\\)", features$featureNames, value=FALSE)
featuresObj = gsub(pattern="[()]", replacement="", x=features[positions,]$featureNames)
features[positions,2] = gsub(pattern="[()]", replacement="", x=features[positions,]$featureNames)

# - Load the library "data.table".
# - Load train data set.
# - Change the colnames of train (setnames function), and select only features with mean and standard deviation (which function).
# - Load trainActivities and trainSubjects data sets.
# - Unify the train data set (cbind function).
library(data.table)
train = read.table(file.path(path,"UCI HAR Dataset/train/X_train.txt"))
data.table::setnames(x=train, old=colnames(train), new=features$featureNames)
train = train[, which(colnames(train) %in% featuresObj)]
trainActivities = read.table(file.path(path,"UCI HAR Dataset/train/Y_train.txt"), col.names=c("Activity"))
trainSubjects = read.table(file.path(path,"UCI HAR Dataset/train/subject_train.txt"), col.names=c("SubjectNum"))
train = cbind(trainSubjects, trainActivities, train)

# - The same steps as above but with the test data set.
test = read.table(file.path(path,"UCI HAR Dataset/test/X_test.txt"))
data.table::setnames(x=test, old=colnames(test), new=features$featureNames)
test = test[, which(colnames(test) %in% featuresObj)]
testActivities = read.table(file.path(path,"UCI HAR Dataset/test/Y_test.txt"), col.names=c("Activity"))
testSubjects = read.table(file.path(path,"UCI HAR Dataset/test/subject_test.txt"), col.names=c("SubjectNum"))
test = cbind(testSubjects, testActivities, test)

# - Merge datasets (rbind function).
# - Change classLabels to activityName (more description names). 
# - Creates a "tidy data" with the average of each variable for each activity and each subject (melt, dcast and write.csv functions)
dataAll = rbind(train, test)
dataAll$Activity = factor(dataAll$Activity, levels=activityLabels$classLabels, labels=activityLabels$activityName)
dataAll$SubjectNum = as.factor(dataAll$SubjectNum)
dataAll = reshape2::melt(data=dataAll, id=c("SubjectNum","Activity"))
dataAll = reshape2::dcast(data=dataAll, SubjectNum + Activity ~ variable, fun.aggregate=mean)
write.table(x=dataAll, file="tidyData.txt", quote=FALSE)


