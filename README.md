# ds-specialization-gcd-03
Files for the Getting and Cleaning Data course of the Coursera Data Science Specialization

This git repository contains the run_analysis.R script which takes as input the data collected from the accelerometers from the Samsung Galaxy S smartphone. That data is compused of two subsets, train and test, which information is collected in the next files:

+ Training/Test subject file: It contains the ID of the subject that has performed the activity
+ Training/Test Y file: It contains the ID of the activity that each subject has performed.
+ Training/Test X file: Contains the results that each subject reported for each activiy. We are interested in mean and standard deviation for these measure. Each column represents a feature.
+ Features file": It has the names of the different features monitored.
+ Activities file: It has information of the activities. Each one has an ID and a name.

The analysis does the next:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

