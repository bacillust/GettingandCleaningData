library(dplyr)

### 1. Merges the training and the test sets to create one data set.
### reading raw data
train_x <- read.table("./train/X_train.txt", header=F)
train_y <- read.table("./train/y_train.txt", header=F)
test_x <- read.table("./test/X_test.txt", header=F)
test_y <- read.table("./test/y_test.txt", header=F)
features <- read.table("./features.txt", header=F)
feature_vec <- as.character(features[,2])
activity_label <- read.table("./activity_labels.txt", header=F)

activity_data <- rbind(train_x, test_x)


######################################################
### 2. Extracts only the measurements on the mean and standard deviation for each measurement.

### get features related with mean or std
mean_idx <- grep("mean()", feature_vec)
std_idx <- grep("std()", feature_vec)
target_idx <- c(mean_idx, std_idx)
target_names <- feature_vec[target_idx]

activity_data2 <- activity_data[, target_idx]

#######################################################
### 3. Uses descriptive activity names to name the activities in the data set

### get label of activity
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

########################################################
### 4. Appropriately labels the data set with descriptive variable names.

### label subject ID and activity
names(activity_data3) <- c("subject_ID", "activity", target_names)
activity_data4 <- tbl_df(activity_data3)


#############################################################
### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
### variable for each activity and each subject.

### get a mean of data for each activity and each subject
by_activity_subject <- group_by(activity_data4, subject_ID, activity)
output_data <- summarise_each(by_activity_subject, funs(mean))
### write a output as a file
write.table(output_data, file="tidyData_activity.txt", row.names = F, col.names = T, sep = "\t", quote=F)
