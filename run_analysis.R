## 1. Merges the training and the test sets to create one data set

# Read the train data set
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Read the test data
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Read the data description
var_name <- read.table("./UCI HAR Dataset/features.txt")

# Read the activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

# Merge training and test set
x_all <- rbind(x_train, x_test)
y_all <- rbind(y_train, y_test)
subject_all <- rbind(subject_train, subject_test)

#--------------------------------------------------------------------

## 2. Extracts only the measurements on the mean 
## and standard deviation for each measurement
selected_var <- var_name[grep("mean\\(\\)|std\\(\\)", var_name[, 2]), ]
x_all <- x_all[, selected_var[, 1]]

#--------------------------------------------------------------------

## 3. Uses descriptive activity names 
## to name the activities in the data set
colnames(y_all) <- "activity"
y_all$activitylabel <- factor(y_all$activity, 
                              labels = as.character(activity_labels[, 2]))
activitylabel <- y_all[, -1]

#--------------------------------------------------------------------

## 4. Appropriately labels the data set
## with descriptive variable names
colnames(x_all) <- var_name[selected_var[, 1], 2]

#--------------------------------------------------------------------

## 5. From the data set in step 4, 
## creates a second, independent tidy data set 
## with the average of each variable for each activity 
## and each subject.

## load dplyr package
library(dplyr)

colnames(subject_all) <- "subject"
all <- cbind(x_all, activitylabel, subject_all)
all_mean <- all %>% 
                group_by(activitylabel, subject) %>% 
                summarize_each(funs(mean))
write.table(all_mean, file = "./UCI HAR Dataset/tidydata.txt", 
            row.names = FALSE, col.names = TRUE)

