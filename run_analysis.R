
# R script for cleaning the 'UCI HAR Dataset' which is available at:
#
# The 'UCI HAR Dataset' is available here:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#
# This Script performs the following.

# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


library(data.table)
library(dplyr)
library(reshape2)


# define the path for Data Files
datafiles = file.path("C:/Users/alok_/Desktop/R Scope/Data Cleansing/Assignment/", "UCI HAR Dataset")
# create a file which will contain the 28 file names
files = list.files(datafiles, recursive=TRUE)

# Load data set of training and test
# Load training tables - xtrain / ytrain, subject train
xtrain = read.table(file.path(datafiles, "train", "X_train.txt"),header = FALSE)
ytrain = read.table(file.path(datafiles, "train", "y_train.txt"),header = FALSE)
subject_train = read.table(file.path(datafiles, "train", "subject_train.txt"),header = FALSE)

# Load the testing tables
xtest = read.table(file.path(datafiles, "test", "X_test.txt"),header = FALSE)
ytest = read.table(file.path(datafiles, "test", "y_test.txt"),header = FALSE)
subject_test = read.table(file.path(datafiles, "test", "subject_test.txt"),header = FALSE)

# Load the features data
features = read.table(file.path(datafiles, "features.txt"),header = FALSE)
features[,2] <- gsub("-", "_", gsub("[()]", "", features[,2]))
features[,2] <- gsub("^f", "frequencyDomain", features[,2])
features[,2] <- gsub("^t", "timeDomain", features[,2])
features[,2] <- gsub("Acc", "Accelerometer", features[,2])
features[,2] <- gsub("Gyro", "Gyroscope", features[,2])
features[,2] <- gsub("Mag", "Magnitude", features[,2])
features[,2] <- gsub("Freq", "Frequency", features[,2])

# Load activity labels data
activitylabels = read.table(file.path(datafiles, "activity_labels.txt"),header = FALSE)

#Update column Values to training Data
colnames(xtrain) = features[,2]
colnames(ytrain) = "activityId"
colnames(subject_train) = "subjectId"
# Update column values to test data
colnames(xtest) = features[,2]
colnames(ytest) = "activityId"
colnames(subject_test) = "subjectId"
# check for the activity labels value
colnames(activitylabels) <- c('activityId','activityType')

# Merge the training and test data
merge_training_data = cbind(ytrain, subject_train, xtrain)
merge_test_data = cbind(ytest, subject_test, xtest)
#Create the complete data table merging both table tables i.e training and test data
complete_data_set = rbind(merge_training_data, merge_test_data)

# read all the values that are available
colNames = colnames(complete_data_set)
# get all the mean and standard deviations and its corresponding activityId and subjectId 
mean_std_data= (grepl("activityId" , colNames) | grepl("subjectId" , colNames) | grepl("mean" , colNames) | grepl("std" , colNames))
# get the required dataset
mean_std_complete_data <- complete_data_set[ , mean_std_data== TRUE]

# merge with labels
data_with_activity_names = merge(mean_std_complete_data, activitylabels, by='activityId', all.x=TRUE)

# Tidy Data Set
tidy_data_set <- aggregate(. ~subjectId + activityType, data_with_activity_names, mean)
tidy_data_set <- tidy_data_set[order(tidy_data_set$subjectId, tidy_data_set$activityId),]

#The last step is to write the ouput to a text file 
write.table(tidy_data_set, "tidy_data_set.txt", row.name=FALSE)
