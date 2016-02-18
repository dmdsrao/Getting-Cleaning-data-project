Getting and Cleaning Data Project

 The data linked from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
 A full description is available at the site where the data was obtained:
 http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
 
 Data Set Information:

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) 
wearing a smartphone (Samsung Galaxy S II) on the waist. 
Using its embedded accelerometer and gyroscope, 
we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. 
The experiments have been video-recorded to label the data manually. 
The obtained dataset has been randomly partitioned into two sets, 
where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width 
sliding windows of 2.56 sec and 50% overlap (128 readings/window). 
The sensor acceleration signal, which has gravitational and body motion components, 
was separated using a Butterworth low-pass filter into body acceleration and gravity. 
The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. 
From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

Attribute Information:

For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

run_analysis.R script does the following

1. Merges the training and the test sets to create one data set.

Read  into tables data in folowing files

features.txt

activity_labels.txt

subject_train.txt

x_train.txt

y_train.txt

subject_test.txt

x_test.txt

y_test.txt

assign column names and merge in one file

 
2. Extracts only the measurements on the mean and standard deviation for each measurement.

  create colnames vector and use grep1 to extract the required columns numbers 
  use the vector of the colum numbers to get the dataset with required columns
  
3. Uses descriptive activity names to name the activities in the data set

  merge activity_labels table to the data
  
4. Appropriately labels the data set with descriptive variable names.

  use gsub to clean data labels and apply appropriate labels

5. creates a second, independent tidy data set with the average of each variable for each activity and each subject.

  use aggregate function to average over each activity and subject and write the final data
