## Author:   Jorge Gómez Robles 
## Email:    gomerudo@gmail.com
##
## This script converts the raw data for human activity recognized using  
## smartphones into tidy data: 
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

library(stringr)
################################################################################
################################ CONSTANTS #####################################
################################################################################

# Download constants
REMOTE_FILE <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
WORKSPACE <- 'workspace/GCD_project'
DATASET_FILE_NAME_ZIP <- 'Dataset.zip'
DATASET_FILE_NAME <- 'UCI HAR Dataset'
FULL_PATH_DATASET_FILE_ZIP <- str_c(WORKSPACE, "/", DATASET_FILE_NAME_ZIP)
FULL_PATH_DATASET_FILE <- str_c(WORKSPACE, "/", DATASET_FILE_NAME)

# Data constants (files)
TRAININGSET_X_FILE <- str_c(FULL_PATH_DATASET_FILE, "/", "train/X_train.txt")
TRAININGSET_Y_FILE <- str_c(FULL_PATH_DATASET_FILE, "/", "train/y_train.txt")
TRAININGSET_SUBJECT_FILE <- str_c(FULL_PATH_DATASET_FILE, "/", "train/subject_train.txt")

TESTSET_X_FILE <- str_c(FULL_PATH_DATASET_FILE, "/", "test/X_test.txt")
TESTSET_Y_FILE <- str_c(FULL_PATH_DATASET_FILE, "/", "test/y_test.txt")
TESTSET_SUBJECT_FILE <- str_c(FULL_PATH_DATASET_FILE, "/", "test/subject_test.txt")

FEATURES_FILE <- str_c(FULL_PATH_DATASET_FILE, "/", "features.txt")
ACTIVITIES_FILE <- str_c(FULL_PATH_DATASET_FILE, "/", "activity_labels.txt")


################################################################################
############################# GLOBAL VARIABLES #################################
################################################################################
trainingSet <- NULL
testSet <- NULL
mergedSet <- NULL

trainingSetY <- NULL
trainingSetX <- NULL
trainingSetSubject <- NULL
testSetX <- NULL
testSetY <- NULL
testSetSubject <- NULL
features <- NULL
activities <- NULL

################################################################################
## printImportantConstants: Print important constants to take in count for this
##                          analysis.
################################################################################
printImportantConstants <- function(){
  message(str_c("[INFO] REMOTE FILE: ", REMOTE_FILE))
  message(str_c("[INFO] WORKSPACE: ", WORKSPACE))
  message(str_c("[INFO] TRAINING X FILE: ", TRAININGSET_X_FILE))
  message(str_c("[INFO] TRAINING Y FILE: ", TRAININGSET_Y_FILE))
  message(str_c("[INFO] TRAINING SUBJECT FILE: ", TRAININGSET_SUBJECT_FILE))
  message(str_c("[INFO] TEST X FILE: ", TESTSET_X_FILE))
  message(str_c("[INFO] TEST Y FILE: ", TESTSET_Y_FILE))
  message(str_c("[INFO] TEST SUBJECT FILE: ", TESTSET_SUBJECT_FILE))
  message(str_c("[INFO] FEATURES FILE: ", FEATURES_FILE))
  message(str_c("[INFO] ACTIVITIES FILE: ", ACTIVITIES_FILE))
}

################################################################################
## prepareData: Function to create workspace and download raw data for analysis
################################################################################
prepareData <- function() {
  if( !file.exists(WORKSPACE) ){
    message("Creating workspace directory..")
    dir.create(WORKSPACE, recursive = TRUE)
  }
  if( !file.exists(FULL_PATH_DATASET_FILE_ZIP) ){
    message("Downloading Data set...")
    download.file(REMOTE_FILE, destfile = FULL_PATH_DATASET_FILE_ZIP, method = "curl")
  }
  unzip(zipfile = FULL_PATH_DATASET_FILE_ZIP, exdir = WORKSPACE)
}

################################################################################
## loadLibraries: This function loads the libraries for the operations
################################################################################
loadLibraries <- function() {
  library(curl)
  library(dplyr)
  library(tidyr)
}


################################################################################
## loadSets: Load the needed data and assign to global variables so they are
##           available for other functions
################################################################################
loadSets <- function() {
  
  features <<- tbl_df( read.table(FEATURES_FILE, col.names = c("ID", "FEATURE_NAME")) )
  activities <<- tbl_df( read.table(ACTIVITIES_FILE, col.names = c("ID", "ACTIVITY_NAME")) )

  trainingSetX <<- tbl_df( read.table(TRAININGSET_X_FILE, col.names = make.unique(as.vector(features$FEATURE_NAME)), check.names = FALSE ) )
  trainingSetY <<- tbl_df( read.table(TRAININGSET_Y_FILE, col.names = "REPORTED_ACTIVITY") )
  trainingSetSubject <<- tbl_df( read.table(TRAININGSET_SUBJECT_FILE, col.names = "SUBJECT") )

  testSetX <<- tbl_df( read.table(TESTSET_X_FILE, col.names = make.unique(as.vector(features$FEATURE_NAME)), check.names = FALSE) )
  testSetY <<- tbl_df( read.table(TESTSET_Y_FILE, col.names = "REPORTED_ACTIVITY") )
  testSetSubject <<- tbl_df( read.table(TESTSET_SUBJECT_FILE, col.names = "SUBJECT") )
  
  # features <<- read.table(FEATURES_FILE, col.names = c("ID", "FEATURE_NAME")) 
  # activites <<- read.table(ACTIVITIES_FILE, col.names = c("ID", "ACTIVITY_NAME")) 
  # 
  # trainingSetX <<- read.table(TRAININGSET_X_FILE, col.names = features$FEATURE_NAME ) 
  # trainingSetY <<- read.table(TRAININGSET_Y_FILE, col.names = "REPORTED_FEATURE") 
  # trainingSetSubject <<- read.table(TRAININGSET_SUBJECT_FILE, col.names = "SUBJECT") 
  # 
  # testSetX <<- read.table(TESTSET_X_FILE)
  # testSetY <<- read.table(TESTSET_Y_FILE) 
  # testSetSubject <<- read.table(TESTSET_SUBJECT_FILE) 
  
}


################################################################################
## buildCustomTrainingSet: 
################################################################################
buildCustomTrainingSet <- function(){
  resultObject <- NULL

  if( is.null(trainingSetX) || is.null(trainingSetY) || is.null(trainingSetSubject) ){
    error("Wrong data set. Please verify you have a valid data set.")
  }
  resultObject <- bind_cols(trainingSetSubject, trainingSetY, trainingSetX)
  resultObject
}

################################################################################
## buildCustomTestSet: 
################################################################################
buildCustomTestSet <- function(){
  resultObject <- NULL
  
  if( is.null(testSetX) || is.null(testSetY) || is.null(testSetSubject) ){
    error("Wrong data set. Please verify you have a valid data set.")
  }
  resultObject <- bind_cols(testSetSubject, testSetY, testSetX)
  resultObject
}

################################################################################
## mergeSets: 
################################################################################
mergeSets <- function(){
  # It adds a new column GROUP to identify each row
  bind_rows("train" = trainingSet, "test" = testSet, .id = "GROUP")
}

collectMeanAndStd <- function(set){
  select(set, GROUP, SUBJECT, REPORTED_ACTIVITY, matches(".*mean\\(\\).*|.*std\\(\\).*"))
  #select(set, matches(".*mean\\(\\).*|.*std\\(\\).*"))
}

replaceActivitiesWithValue <- function(set){
  mutate(set, REPORTED_ACTIVITY = activities$ACTIVITY_NAME[ID=REPORTED_ACTIVITY] )
}

makeVariablesDescriptive <- function(set){
  names(set)<-gsub("^t", "TIME_", names(set))
  names(set)<-gsub("^f", "FREQUENCY_", names(set))
  names(set)<-gsub("Acc", "ACCELEROMETER_", names(set))
  names(set)<-gsub("Gyro", "GYROSCOPE_", names(set))
  names(set)<-gsub("Mag", "MAGNITUDE_", names(set))
  names(set)<-gsub("BodyBody", "Body", names(set))
  names(set)<-gsub("Body", "BODY_", names(set))
  names(set)<-gsub("Jerk", "JERK_", names(set))
  names(set)<-gsub("-mean\\(\\)", "MEAN", names(set))
  names(set)<-gsub("-std\\(\\)", "STD", names(set))
  names(set)<-gsub("-", "_", names(set))
  set
}
################################################################################
## doAnalysis: The main method. Calls the other functions to run the analysis.
################################################################################
doAnalysis <- function() {
  # Pre tasks to start the assignment 
  loadLibraries()
  prepareData()
  printImportantConstants()
  loadSets()
  
  # 1. Merges the training and the test sets to create one data set.
  trainingSet <<- buildCustomTrainingSet()
  testSet <<- buildCustomTestSet()
  mergedSet <<- mergeSets()
  
  # 2. Extracts only the measurements on the mean and standard deviation for 
  #    each measurement. 
  mergedSet <<- collectMeanAndStd(mergedSet)
  
  # 3. Uses descriptive activity names to name the activities in the data set
  mergedSet <<- replaceActivitiesWithValue(mergedSet)
  
  # 4. Appropriately labels the data set with descriptive variable names. 
  tidyData <<- makeVariablesDescriptive(mergedSet)
  
  # 5. From the data set in step 4, creates a second, independent tidy data set 
  #    with the average of each variable for each activity and each subject.
  tidyDataAvg <<- select(tidyData, -GROUP)
  tidyDataAvg <<- aggregate(. ~SUBJECT + REPORTED_ACTIVITY, tidyDataAvg, mean)
  tidyDataAvg <<- arrange(tidyDataAvg, SUBJECT, REPORTED_ACTIVITY)
}
