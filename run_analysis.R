## This script performs the following functions USING dplyr:

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each 
# measurement. 
# 3. Uses descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

library(plyr)
library(dplyr)

# Step 1: Read in and merge training and test datasets.
# Be sure to place extracted data folder (UCI HAR Dataset) in working directory.
traindata<-read.table("~/UCI HAR Dataset/train/X_train.txt",header=F,sep="")
testdata<-read.table("~/UCI HAR Dataset/test/X_test.txt",header=F,sep="")

# Merge training and test data by appending, since variables are identical:
fulldata<-rbind(traindata,testdata)

# Step 2: Extract only mean/std measurements.
# First reads in feature name vector, features.txt:
feature_names<-read.table("~/UCI HAR Dataset/features.txt",header=F,sep="")

# From featuresinfo.txt, define feature names containing "mean()" or "std()" 
# as labeling the required mean/std variable. Find their row indices:
mean_indices<-grep("mean()",feature_names$V2,fixed=T)
std_indices<-grep("std()",feature_names$V2,fixed=T)
# Combine and sort both sets of indices:
indices_unsorted<-append(mean_indices,std_indices)
indices<-indices_unsorted[order(indices_unsorted)]
# Set mean_std_data to contain only these variables:
mean_std_data<-fulldata[,indices]

# Step 3: Relabel activities with names.
# Create activity column from y_train and y_test:
y_train<-read.table("~/UCI HAR Dataset/train/y_train.txt",header=F,sep="")
y_test<-read.table("~/UCI HAR Dataset/test/y_test.txt",header=F,sep="")
activity_labels<-read.table("~/UCI HAR Dataset/activity_labels.txt",header=F,sep="")
activity_code<-rbind(y_train,y_test)
#Convert activity codes to factors and relabel using activity_labels.txt:
activity_code[,1]<-as.factor(activity_code[,1])
activity_labels<-read.table("~/UCI HAR Dataset/activity_labels.txt",header=F,sep="")
levels(activity_code[,1]) <- activity_labels$V2
# Finally, append to first column of mean_std_data, labeled "ACTIVITY":
ACTIVITY<-activity_code[,1]
mean_std_data<-cbind(ACTIVITY,mean_std_data)

# Step 4: Relabel variable with descriptive names, taken from feature_names.
names(mean_std_data)<-c("ACTIVITY",as.character(feature_names[c(indices),2]))

# Step 5: Creates a second tidy data set with the average of each variable.
# Create subject column from subject_train and subject_test:
subject_train<-read.table("~/UCI HAR Dataset/train/subject_train.txt",header=F,sep="")
subject_test<-read.table("~/UCI HAR Dataset/test/subject_test.txt",header=F,sep="")
SUBJECT<-rbind(subject_train,subject_test)[,1]
mean_std_data<-cbind(SUBJECT,mean_std_data)

# Remove any missing values and convert to plyr format:
tidydata <- tbl_df(mean_std_data[complete.cases(mean_std_data),])

    
grouped_table<-group_by(tidydata,SUBJECT,ACTIVITY)
df<-summarise_each(grouped_table,funs(mean))
write.table(df,"df.txt",row.name=FALSE)
