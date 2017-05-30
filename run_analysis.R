run_analysis <- function(){

  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  filename <- "samsung_galaxy_dataset.zip"
  
  ## if the file doesn't existe, then download and unzip it
  if(!file.exists(filename)){
    download.file(fileurl, destfile = filename, method = "curl")
    unzip(filename)
  }
  ##load de activity labels and futures files
  activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
  features <- read.table("./UCI HAR Dataset/features.txt")
  
  ## select measurements on the mean and standard deviation
  features_needed <- grep(".*mean.*|.*std.*", as.character(features[, 2]))
  features_needed_names<- features[features_needed, 2]
  
  ## delete the () within the names
  features_needed_names<- gsub("[()]", "", features_needed_names)
  
  ##load the data sets
  training_set <- read.table("UCI HAR Dataset/train/X_train.txt")
  training_set <- training_set[features_needed]
  training_labels <- read.table("UCI HAR Dataset/train/y_train.txt")
  training_subjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
  training <- cbind(training_subjects, training_labels, training_set)
  test_set <- read.table("UCI HAR Dataset/test/X_test.txt")
  test_set <- test_set[features_needed]
  test_labels <- read.table("UCI HAR Dataset/test/y_test.txt")
  test_subjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
  test <- cbind(test_subjects, test_labels, test_set)

  ##Merge the training and the test sets to create one data set
  merge_data <- rbind(training, test)
  
  ## define the colnames for merge data and add activity labels  
  colnames(merge_data) <- c("subject", "activity", features_needed_names)
  merge_data$activity <- factor(merge_data$activity, levels = activity_labels[, 1], labels = activity_labels[, 2])
  merge_data$subject <- as.factor(merge_data$subject)
  
  ##From the data set merge_data, creates an independent tidy data set 
  ##with the average of each variable for each activity and each subject.
  data_final <- melt(merge_data, id=c("subject", "activity"))
  data_final_mean <- dcast(data_final, subject + activity ~ variable, mean)
  
  ## order by subject and activity
  data_final_mean <- data_final_mean[order(data_final_mean$subject, desc(data_final_mean$activity)), ]
  
  ## save the tidy file
  write.csv(data_final_mean, "./tidy_dataset_assignment.txt", row.names = FALSE)
  
  }