PeerAssessments
===============

This repository is created for Coursera PeerAssessments project

How script run_analysis.R works:

##################################################################
## Script Name: run_analysis.R
## Function: 
## This script does the following for the Peer Assessment project
## -- Merges the training and test sets to create one data set
## -- Extracts only the measurements on the mean and standard deviation for each measurement
## -- Uses descriptive activity names instead of the activity lable in the data set
## -- Appropriately labels the data set with descriptive variabel names
## -- Creates a second, independent tidy data set with the average of each varialbe for each
##    activity and each subject.
## -- Write the tidy data set to a csv format text file tidy_dataset.txt
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

##################################################################
## step1-2: Load activity_df and define a function to convert activity_lable to activity_name

##################################################################
## step1-2a: Read in file activity_labels.txt

##################################################################
## step1-2b: Define function get_activityname()
##          -- input parameter: activity_lable     
##          -- output: corresponding activity_name



##################################################################
## step1-3: Merge test data from subject_test.txt, X_test.txt, and y_test.txt 



##################################################################
## step1-3a: Read in X_test.txt and extract columns based on selected features

##################################################################
## step1-3b: Read in subject_test.txt and y_test.txt 

##################################################################
## step1-3c: Convert ytest_df from activity_label to activity_name

##################################################################
## step1-3d: Merge 3 test data frames



##################################################################
## step1-4: Merge train data from subject_train.txt, X_train.txt, and y_train.txt 


##################################################################
## step1-4a: Read in X_train.txt and extract columns based on selected features

##################################################################
## step1-4b: Read in subject_train.txt and y_train.txt

##################################################################
## step1-4c: Convert activity_df from activity_label to activity_name

##################################################################
## step1-4d: Merge 3 train data frames



##################################################################
## step1-5: Merge train and test data frames



##################################################################
## step2: Create a second data set with averages of each variable for each activity and each subject



##################################################################
## step3: write the data set to a text file

