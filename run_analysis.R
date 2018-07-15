> # run_analysis.R
> 
> # we should create one R script called run_analysis.R that does the following.
> 
> ############ initial step (A)
> 
> library(dplyr)
> 
> # getting data by download the file if it does not exist
> zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
> zipFile <- "UCI HAR Dataset.zip"
> if (!file.exists(zipFile)) {
+     download.file(zipUrl, zipFile, mode = "wb")
+   }
> # unzipping the file 
> dataPath <- "UCI HAR Dataset"
> if (!file.exists(dataPath)) {
+  unzip(zipFile)
+ }
> 
> ########## initial step (B)
> 
> # reading training data
> trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
> trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
> trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))
> 
> # reading test data 
> testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
> testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
> testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))
> 
> # reading features
> features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
> 
> # reading activity labels
> activities <- read.table(file.path(dataPath, "activity_labels.txt"))
> colnames(activities) <- c("activityId", "activityLabel")
> 
> ############## step1
> 
> humanActivity <- rbind(
+    cbind(trainingSubjects, trainingValues, trainingActivity),
+    cbind(testSubjects, testValues, testActivity)
+  )
> 
> # to save memory, remove individual data tables 
> rm(trainingSubjects, trainingValues, trainingActivity, testSubjects, testValues, testActivity)
> 
> colnames(humanActivity) <- c("subject", features[, 2], "activity")
> 
> ############ step2
> 
> # determine columns by column name
> columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
> 
> # keeping data based on these columns 
> humanActivity <- humanActivity[, columnsToKeep]
> ############ Step 3
> 
> humanActivity$activity <- factor(humanActivity$activity, levels = activities[, 1], labels = activities[, 2])
> 
> ############ Step 4
> 
> humanActivityCols <- colnames(humanActivity)
> 
> # remove special characters
> humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)
> 
> # expand abbreviations and clean up names
> humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
> humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
> humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
> humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
> humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
> humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
> humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
> humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
> 
> humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)
> 
> # use new labels as column names
> colnames(humanActivity) <- humanActivityCols
> 
> ############# Step 5
> 
> # grouping in terms of subject and activity and summarise by mean
> humanActivityMeans <- humanActivity %>% group_by(subject, activity) %>% 
+ summarise_all(funs(mean))
> 
> # output to file "tidy_data.txt"
> write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE,quote = FALSE)
