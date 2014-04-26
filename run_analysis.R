##################################################################
## Script Name: run_analysis.R
## Function: 
## This script does the following for the Peer Assessment project
## -- Merges the training and test sets to create one data set
## -- Extracts only the measurements on the mean and standard deviation for each measurement
## -- Uses descriptive activity names instead of the activity label in the data set
## -- Appropriately labels the data set with descriptive variable names
## -- Creates a second, independent tidy data set with the average of each varialbe for each
##    activity and each subject.
## -- Writes the tidy data set to a csv format text file
##################################################################



##################################################################
## step0: Assumptions before running this script
##
## -- zip file is downloaded in the working directory and unzipped
## -- packages reshape and reshape2 installed and loaded
## install.packages(reshape)
## library(reshape)
## install.packages(reshape2)
## library(reshape2)


##################################################################
## step1: Merge the test and training data sets to create one data set

##################################################################
## step1-1: Read in file features.txt and extract mean and standard deviation measurements

feature_df <- read.table("./UCI HAR Dataset/features.txt", header=FALSE)
sel_mean <- grep("mean",feature_df[,2])
sel_std <- grep("std", feature_df[,2])
subfeature_df <- feature_df[c(sel_mean,sel_std),]
dup <- duplicated(subfeature_df[,2])
subfeature_df <- subfeature_df[!dup,]
subfeature_df[,2] <- as.character(subfeature_df[,2])


##################################################################
## step1-2: Load activity_df and define a function to convert activity_lable to activity_name


##################################################################
## step1-2a: Read in file activity_labels.txt

activity_df <- read.table("./UCI HAR Dataset/activity_labels.txt", header=FALSE)

##################################################################
## step1-2b: Define function get_activityname()
##          -- input parameter: activity_lable     
##          -- output: corresponding activity_name

get_activityname <- function(activity_label) {
	activity_df[activity_label,2]	
}

##################################################################
## step1-3: Merge test data from subject_test.txt, X_test.txt, and y_test.txt 

##################################################################
## step1-3a: Read in X_test.txt and extract columns based on selected features

colnames_v <- as.character(feature_df[,2])
Xtest_df <- read.table("./UCI HAR Dataset/test/X_test.txt", header=FALSE)
colnames(Xtest_df) <- colnames_v
subXtest_df <- Xtest_df[,subfeature_df[,2]]

##################################################################
## step1-3b: Read in subject_test.txt and y_test.txt 

subjecttest_df <- read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE)
ytest_df <- read.table("./UCI HAR Dataset/test/y_test.txt", header=FALSE)


##################################################################
## step1-3c: Convert ytest_df from activity_label to activity_name

ytest_df[,1] <- apply(ytest_df, 2, get_activityname)


##################################################################
## step1-3d: Merge 3 test data frames

test_df <- cbind(subjecttest_df[,1], ytest_df[,1], subXtest_df)
test_df[,2] <- as.character(test_df[,2])
colnames(test_df)[1] <- "Subject"
colnames(test_df)[2] <- "Activity"


##################################################################
## step1-4: Merge train data from subject_train.txt, X_train.txt, and y_train.txt 

##################################################################
## step1-4a: Read in X_train.txt and extract columns based on selected features

Xtrain_df <- read.table("./UCI HAR Dataset/train/X_train.txt", header=FALSE)
colnames(Xtrain_df) <- colnames_v
subXtrain_df <- Xtrain_df[,subfeature_df[,2]]

##################################################################
## step1-4b: Read in subject_train.txt and y_train.txt
subjecttrain_df <- read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE)
ytrain_df <- read.table("./UCI HAR Dataset/train/y_train.txt", header=FALSE)


##################################################################
## step1-4c: Convert activity_df from activity_label to activity_name

ytrain_df[,1] <- apply(ytrain_df, 2, get_activityname)


##################################################################
## step1-4d: Merge 3 train data frames

train_df <- cbind(subjecttrain_df[,1], ytrain_df[,1], subXtrain_df)
train_df[,2] <- as.character(train_df[,2])
colnames(train_df)[1] <- "Subject"
colnames(train_df)[2] <- "Activity"



##################################################################
## step1-5: Merge test and train data frames

merge_df <- rbind(test_df, train_df)


##################################################################
## step2: Create a second data set with averages of each variable for each activity and each subject

library(reshape)
library(reshape2)
melt_df <- melt(merge_df, id=c("Subject", "Activity"), measure.vars=c(-1,-2))
mean_df <- dcast(melt_df, Subject + Activity ~ variable,mean)


##################################################################
## step3: write the data set to a text file

#write.csv(mean_df,file="tidy_dataset.txt",row.names=FALSE)

