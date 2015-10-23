run_analysis <- function(){
  X_test <- read.table("test/X_test.txt")
  y_test <- read.table("test/y_test.txt")
  
  X_train <- read.table("train/X_train.txt")
  y_train <- read.table("train/y_train.txt")
  
  activity_labels <- read.table("activity_table.txt")
  features <- read.table("features.txt")
  features_char <- as.character(features[,2])
  
  ##Adding labels to X data sets
  colnames(X_test) <- features_char
  colnames(X_train) <- features_char
  
  ##Merging activity labels with y values
  y_test <- merge(activity_labels,y_test,by.y="V1")
  y_train <- merge(activity_labels,y_train,by.y="V1")
  
  ##Bringing in subject IDs
  y_test <- cbind(subject_test, y_test)
  y_train <- cbind(subject_train, y_train)
  
  #Removing activity label ID
  y_train <- y_train[,-c(2)] 
  y_test <- y_test[,-c(2)] 
  
  ##Bring columns together
  train <- cbind(y_train,X_train)
  test <- cbind(y_test, X_test)
  
  ##Combine test & train
  combined <- rbind(test, train)
  valid_column_names <- make.names(names=names(combined), unique=TRUE, allow_ = TRUE)
  names(combined) <- valid_column_names
  
  ##Separate and analyze data
  tidy_data <- select(combined, Subject.ID, Activity, contains("mean"), contains("std"))
  summary <- tidy_data %>% group_by(Subject.ID, Activity) %>% summarise_each(funs(mean))
  summary
}