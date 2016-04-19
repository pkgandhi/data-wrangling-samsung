library(tidyr)
library(dplyr)

### 0: Loading all the data

basedir <- "C:/Users/Pratik Gandhi/Documents/Data Science Stuff/Foundation_DS/UCI_HAR_Dataset/"
setwd(basedir)

## Loading the test dataset
setwd(paste0(basedir,"test"))
test_files <- list.files(paste0(basedir,"test"))
X_test <- read.table(test_files[grepl("X_test.txt$",test_files)])
y_test <- read.table(test_files[grepl("y_test.txt$",test_files)])
subject_test <- read.table(test_files[grepl("subject_test.txt$",test_files)])

## Loading the train dataset
setwd(paste0(basedir,"train"))
train_files <- list.files(paste0(basedir,"train"))
X_train <- read.table(train_files[grepl("X_train.txt$",train_files)])
y_train <- read.table(train_files[grepl("y_train.txt$",train_files)])
subject_train <- read.table(train_files[grepl("subject_train.txt$",train_files)])

## Loading the activity labels and features
setwd(basedir)
base_files <- list.files(basedir)
features_file <- read.table(base_files[grepl("features.txt",base_files)])
activity_labels <- read.table(base_files[grepl("activity_labels.txt",base_files)])



### 1: Merge the training and the test sets to create one data set.

X_combined <- rbind(X_train, X_test)
y_combined <- rbind(y_train, y_test)
subject_combined <- rbind(subject_train, subject_test)



### 2:Extracts columns containing mean and standard deviation for each measurement

feature_names <- features_file$V2
feature_names <- make.names(feature_names,unique = TRUE)

## Assigning the values to our dataset
colnames(X_combined) <- feature_names

init_mean_std_data <- X_combined %>% select( matches("(mean|std)"))
#col_mean <- grep("(mean|std)", names(X_combined), value = TRUE)
#mean_cols <- X_combined[,col_mean]



### 3:Creates variables called ActivityLabel and ActivityName that label all observations with the corresponding activity labels and names respectively

names(activity_labels) <- c("ActivityLabel", "ActivityName")
names(subject_combined) <- c("subject")
names(y_combined) <- c("ActivityLabel")
total_data <- cbind(subject_combined, y_combined) %>% left_join(activity_labels) %>% cbind(X_combined)



### 4: From the data set in step 3, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

mean_std_data <- total_data %>% select( matches("(mean|std)"))
tidy_data <- total_data %>% group_by(subject, ActivityName) %>% summarise_each(funs(mean), -one_of(c('subjet', 'ActivityLabel', 'ActivityName')))