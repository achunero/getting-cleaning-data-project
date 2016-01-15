##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## Krishna Kumar
## 01-15-2016

# runAnalysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################


# Clean up workspace
rm(list=ls())

library(plyr)
## Set the path to the location of the Project articles/files 
setwd("D:/Programs/R programming/Getting and Cleaning Data/Project/UCI HAR Dataset")

##Read the activities and the features text files
activity <- read.table("activity_labels.txt",header=FALSE)
features <- read.table("features.txt",head=FALSE)

##Read the train datasets
Trainsub <- read.table("./train/subject_train.txt")
Xtrain <- read.table("./train/X_train.txt",header=FALSE)
Ytrain <- read.table("./train/y_train.txt",header=FALSE)

##Assign appropriate names to the column for activty,features 
##and Ytrain data frames
colnames(activity) <- c("activityid","activity.name")
colnames(Trainsub)<- c("subjectid")
colnames(Ytrain)<-c("activityid")

##Before assigning column names to the Xtrain data frame, make sure to
##refine the names from features dataset and store the mean and std variable 
##names to a new vector (new.names)
features.names <- gsub("-","",features[,2])
new.xtrain.names <- gsub("[()]","",features.names)
xtrain.colnames <- new.xtrain.names[grep(".*mean.*|.*std.*",new.xtrain.names)]
colnames(Xtrain) <- new.xtrain.names

##Select the columns(mean and std) from Xtrain dataframe and store in new df
finalXtrain <- subset(Xtrain,select=xtrain.colnames)

##Column bind the Subject,Xtrain and Ytrain dataframe
Train <- cbind(Trainsub,Ytrain,finalXtrain)

##Read the test text files
Testsub <-read.table("./test/subject_test.txt",head=FALSE)
Xtest <- read.table("./test/X_test.txt",header=FALSE)
Ytest <- read.table("./test/y_test.txt",header=FALSE)

##Assign appropriate column names to the dataset
colnames(Testsub) <- c("subjectid")
colnames(Ytest) <- c("activityid")
colnames(Xtest) <- new.xtrain.names

##Select the columns(mean and std) from Xtest dataframe and store in new df
finalXtest <- subset(Xtest,select=xtrain.colnames)

##Column bind the Subject,Xtest and Ytest dataframe
Test <- cbind(Testsub,Ytest,finalXtest)

##Row bind the Train and Test dataset to form the final merged dataset
Final <- rbind(Train,Test)

##Uses descriptive activity names to name the activities in the data set
activity[,2]<-as.character(activity[,2])
Final$activityid<-factor(Final$activityid,levels=activity[,1],labels=activity[,2])
Final$subjectid<-as.factor(Final$subjectid)

##From the above data set , create a second, independent tidy data 
##set with the average of each variable for each activity and each subject
Output.tidy <- ddply(Final,.(subjectid,activityid),function(x)colMeans(x[,3:81]))

Output.tidy <- rename(Output.tidy,c("subjectid"="Subject","activityid"="Activity"))

##Write the final output to a file
write.table(Output.tidy,file="./tidyData-output.txt",sep=",",row.names=FALSE)
