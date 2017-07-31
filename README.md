# Getting and Cleaning Data Course Project

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project.
You will be required to submit: 
1) a tidy data set as described below
2) a link to a Github repository with your script for performing the analysis
3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 
4) README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

# R Code Explanation

## 1. Merges the training and the test sets to create one data set.

train_x <- read.table("./train/X_train.txt", header=F)
train_y <- read.table("./train/y_train.txt", header=F)
test_x <- read.table("./test/X_test.txt", header=F)
test_y <- read.table("./test/y_test.txt", header=F)
features <- read.table("./features.txt", header=F)
feature_vec <- as.character(features[,2])
activity_label <- read.table("./activity_labels.txt", header=F)

activity_data <- rbind(train_x, test_x)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

mean_idx <- grep("mean()", feature_vec)
std_idx <- grep("std()", feature_vec)
target_idx <- c(mean_idx, std_idx)
target_names <- feature_vec[target_idx]

activity_data2 <- activity_data[, target_idx]

## 3. Uses descriptive activity names to name the activities in the data set

train_label <- train_y
test_label <- test_y 

for (i in 1:dim(activity_label)[1])
{
        train_label[which(train_y == i),1] <- as.character(activity_label[i, 2])
        test_label[which(test_y == i),1] <- as.character(activity_label[i, 2])
}

activity_label <- rbind(train_label, test_label)

train_subject <- read.table("./train/subject_train.txt", header=F)
test_subject <- read.table("./test/subject_test.txt", header=F)
subject_ID <- rbind(train_subject, test_subject)

activity_data3 <- cbind(subject_ID, activity_label, activity_data2)

## 4. Appropriately labels the data set with descriptive variable names.

names(activity_data3) <- c("subject_ID", "activity", target_names)
activity_data4 <- tbl_df(activity_data3)

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
by_activity_subject <- group_by(activity_data4, subject_ID, activity)
output_data <- summarise_each(by_activity_subject, funs(mean))
write.table(output_data, file="tidyData_activity.txt", row.names = F, col.names = T, sep = "\t", quote=F)


