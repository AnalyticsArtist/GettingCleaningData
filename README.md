Coursera Data Science - Getting & Cleaning Data Peer Assessment Code Book
=========================================================================

**Narrative**

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis.

**Background**

One of the most exciting areas in all of data science right now is wearable computing - see for example  this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced  algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.

**Analysis Steps**

* Load Libraries & Define Data Path
* Merge the Training and the Test Sets to Create One Data Set
* Extract Only the Measurements on the Mean and Standard Deviation for Each Measurement
* Use Descriptive Activity Names to Name the Activities in the Data Set
* Appropriately Label the Data Set with Descriptive Activity Names
* Create a Second, Independent Tidy Data Set with the Average of Each Variable for Each Activity and Each Subject


**Variable Descriptions**

* subject_id: Identification number for each subject.
* activity_cd: Activity code for each activity.
* activity: Activity label
* tBodyAcc-mean()-X:  Mean value of tBodyAcc for X
* tBodyAcc-mean()-Y:  Mean value of tBodyAcc for Y
* tBodyAcc-mean()-Z:  Mean value of tBodyAcc for Z
* tBodyAcc-std()-X:  Standard deviation of tBodyAcc for X
* tBodyAcc-std()-Y:  Standard deviation of tBodyAcc for Y
* tBodyAcc-std()-Z:  Standard deviation of tBodyAcc for Z
* tBodyAcc-mad()-X:  Median absolute deviation  of tBodyAcc for X
* tBodyAcc-mad()-Y:  Median absolute deviation  of tBodyAcc for Y
* tBodyAcc-mad()-Z:  Median absolute deviation  of tBodyAcc for Z
* tBodyAcc-max()-X:  Largest value in array of tBodyAcc for X
* tBodyAcc-max()-Y:  Largest value in array of tBodyAcc for Y
* tBodyAcc-max()-Z:  Largest value in array of tBodyAcc for Z
* tBodyAcc-min()-X:  Smallest value in array of tBodyAcc for X
* tBodyAcc-min()-Y:  Smallest value in array of tBodyAcc for Y
* tBodyAcc-min()-Z:  Smallest value in array of tBodyAcc for Z
* tBodyAcc-sma():  Signal magnitude area of tBodyAcc 
* tBodyAcc-energy()-X:  Energy measure. Sum of the squares divided by the number of values.  of tBodyAcc for X
* tBodyAcc-energy()-Y:  Energy measure. Sum of the squares divided by the number of values.  of tBodyAcc for Y
* tBodyAcc-energy()-Z:  Energy measure. Sum of the squares divided by the number of values.  of tBodyAcc for Z
* tBodyAcc-iqr()-X:  Interquartile range  of tBodyAcc for X
* tBodyAcc-iqr()-Y:  Interquartile range  of tBodyAcc for Y
* tBodyAcc-iqr()-Z:  Interquartile range  of tBodyAcc for Z
* tBodyAcc-entropy()-X:  Signal entropy of tBodyAcc for X
* tBodyAcc-entropy()-Y:  Signal entropy of tBodyAcc for Y
* tBodyAcc-entropy()-Z:  Signal entropy of tBodyAcc for Z
* tBodyAcc-arCoeff()-X,1:  Autorregresion coefficients with Burg order equal to 4 of tBodyAcc for X,1
* tBodyAcc-arCoeff()-X,2:  Autorregresion coefficients with Burg order equal to 4 of tBodyAcc for X,2
* tBodyAcc-arCoeff()-X,3:  Autorregresion coefficients with Burg order equal to 4 of tBodyAcc for X,3
* tBodyAcc-arCoeff()-X,4:  Autorregresion coefficients with Burg order equal to 4 of tBodyAcc for X,4
* tBodyAcc-arCoeff()-Y,1:  Autorregresion coefficients with Burg order equal to 4 of tBodyAcc for Y,1
* tBodyAcc-arCoeff()-Y,2:  Autorregresion coefficients with Burg order equal to 4 of tBodyAcc for Y,2
* tBodyAcc-arCoeff()-Y,3:  Autorregresion coefficients with Burg order equal to 4 of tBodyAcc for Y,3
* tBodyAcc-arCoeff()-Y,4:  Autorregresion coefficients with Burg order equal to 4 of tBodyAcc for Y,4
* tBodyAcc-arCoeff()-Z,1:  Autorregresion coefficients with Burg order equal to 4 of tBodyAcc for Z,1
* tBodyAcc-arCoeff()-Z,2:  Autorregresion coefficients with Burg order equal to 4 of tBodyAcc for Z,2
* tBodyAcc-arCoeff()-Z,3:  Autorregresion coefficients with Burg order equal to 4 of tBodyAcc for Z,3
* tBodyAcc-arCoeff()-Z,4:  Autorregresion coefficients with Burg order equal to 4 of tBodyAcc for Z,4
* tBodyAcc-correlation()-X,Y:  correlation coefficient between two signals of tBodyAcc for X,Y
* tBodyAcc-correlation()-X,Z:  correlation coefficient between two signals of tBodyAcc for X,Z
* tBodyAcc-correlation()-Y,Z:  correlation coefficient between two signals of tBodyAcc for Y,Z
* tGravityAcc-mean()-X:  Mean value of tGravityAcc for X
* tGravityAcc-mean()-Y:  Mean value of tGravityAcc for Y
* tGravityAcc-mean()-Z:  Mean value of tGravityAcc for Z
* tGravityAcc-std()-X:  Standard deviation of tGravityAcc for X
* tGravityAcc-std()-Y:  Standard deviation of tGravityAcc for Y
* tGravityAcc-std()-Z:  Standard deviation of tGravityAcc for Z
* tGravityAcc-mad()-X:  Median absolute deviation of tGravityAcc for X
* tGravityAcc-mad()-Y:  Median absolute deviation of tGravityAcc for Y
* tGravityAcc-mad()-Z:  Median absolute deviation of tGravityAcc for Z
* tGravityAcc-max()-X:  Largest value in array of tGravityAcc for X
* tGravityAcc-max()-Y:  Largest value in array of tGravityAcc for Y
* tGravityAcc-max()-Z:  Largest value in array of tGravityAcc for Z
* tGravityAcc-min()-X:  Smallest value in array of tGravityAcc for X
* tGravityAcc-min()-Y:  Smallest value in array of tGravityAcc for Y
* tGravityAcc-min()-Z:  Smallest value in array of tGravityAcc for Z
* tGravityAcc-sma():  Signal magnitude area of tGravityAcc 
* tGravityAcc-energy()-X:  Energy measure. Sum of the squares divided by the number of values.  of tGravityAcc for X
* tGravityAcc-energy()-Y:  Energy measure. Sum of the squares divided by the number of values.  of tGravityAcc for Y
* tGravityAcc-energy()-Z:  Energy measure. Sum of the squares divided by the number of values.  of tGravityAcc for Z
* tGravityAcc-iqr()-X:  Interquartile range of tGravityAcc for X
* tGravityAcc-iqr()-Y:  Interquartile range of tGravityAcc for Y
* tGravityAcc-iqr()-Z:  Interquartile range of tGravityAcc for Z
* tGravityAcc-entropy()-X:  Signal entropy of tGravityAcc for X
* tGravityAcc-entropy()-Y:  Signal entropy of tGravityAcc for Y
* tGravityAcc-entropy()-Z:  Signal entropy of tGravityAcc for Z
* tGravityAcc-arCoeff()-X,1:  Autorregresion coefficients with Burg order equal to 4 of tGravityAcc for X,1
* tGravityAcc-arCoeff()-X,2:  Autorregresion coefficients with Burg order equal to 4 of tGravityAcc for X,2
* tGravityAcc-arCoeff()-X,3:  Autorregresion coefficients with Burg order equal to 4 of tGravityAcc for X,3
* tGravityAcc-arCoeff()-X,4:  Autorregresion coefficients with Burg order equal to 4 of tGravityAcc for X,4
* tGravityAcc-arCoeff()-Y,1:  Autorregresion coefficients with Burg order equal to 4 of tGravityAcc for Y,1
* tGravityAcc-arCoeff()-Y,2:  Autorregresion coefficients with Burg order equal to 4 of tGravityAcc for Y,2
* tGravityAcc-arCoeff()-Y,3:  Autorregresion coefficients with Burg order equal to 4 of tGravityAcc for Y,3
* tGravityAcc-arCoeff()-Y,4:  Autorregresion coefficients with Burg order equal to 4 of tGravityAcc for Y,4
* tGravityAcc-arCoeff()-Z,1:  Autorregresion coefficients with Burg order equal to 4 of tGravityAcc for Z,1
* tGravityAcc-arCoeff()-Z,2:  Autorregresion coefficients with Burg order equal to 4 of tGravityAcc for Z,2
* tGravityAcc-arCoeff()-Z,3:  Autorregresion coefficients with Burg order equal to 4 of tGravityAcc for Z,3
* tGravityAcc-arCoeff()-Z,4:  Autorregresion coefficients with Burg order equal to 4 of tGravityAcc for Z,4
* tGravityAcc-correlation()-X,Y:  correlation coefficient between two signals of tGravityAcc for X,Y
* tGravityAcc-correlation()-X,Z:  correlation coefficient between two signals of tGravityAcc for X,Z
* tGravityAcc-correlation()-Y,Z:  correlation coefficient between two signals of tGravityAcc for Y,Z
* tBodyAccJerk-mean()-X:  Mean value of tBodyAccJerk for X
* tBodyAccJerk-mean()-Y:  Mean value of tBodyAccJerk for Y
* tBodyAccJerk-mean()-Z:  Mean value of tBodyAccJerk for Z
* tBodyAccJerk-std()-X:  Standard deviation of tBodyAccJerk for X
* tBodyAccJerk-std()-Y:  Standard deviation of tBodyAccJerk for Y
* tBodyAccJerk-std()-Z:  Standard deviation of tBodyAccJerk for Z
* tBodyAccJerk-mad()-X:  Median absolute deviation of tBodyAccJerk for X
* tBodyAccJerk-mad()-Y:  Median absolute deviation of tBodyAccJerk for Y
* tBodyAccJerk-mad()-Z:  Median absolute deviation of tBodyAccJerk for Z
* tBodyAccJerk-max()-X:  Largest value in array of tBodyAccJerk for X
* tBodyAccJerk-max()-Y:  Largest value in array of tBodyAccJerk for Y
* tBodyAccJerk-max()-Z:  Largest value in array of tBodyAccJerk for Z
* tBodyAccJerk-min()-X:  Smallest value in array of tBodyAccJerk for X
* tBodyAccJerk-min()-Y:  Smallest value in array of tBodyAccJerk for Y
* tBodyAccJerk-min()-Z:  Smallest value in array of tBodyAccJerk for Z
* tBodyAccJerk-sma():  Signal magnitude area of tBodyAccJerk 
* tBodyAccJerk-energy()-X:  Energy measure. Sum of the squares divided by the number of values.  of tBodyAccJerk for X
* tBodyAccJerk-energy()-Y:  Energy measure. Sum of the squares divided by the number of values.  of tBodyAccJerk for Y
* tBodyAccJerk-energy()-Z:  Energy measure. Sum of the squares divided by the number of values.  of tBodyAccJerk for Z
* tBodyAccJerk-iqr()-X:  Interquartile range of tBodyAccJerk for X
* tBodyAccJerk-iqr()-Y:  Interquartile range of tBodyAccJerk for Y
* tBodyAccJerk-iqr()-Z:  Interquartile range of tBodyAccJerk for Z
* tBodyAccJerk-entropy()-X:  Signal entropy of tBodyAccJerk for X
* tBodyAccJerk-entropy()-Y:  Signal entropy of tBodyAccJerk for Y
* tBodyAccJerk-entropy()-Z:  Signal entropy of tBodyAccJerk for Z
* tBodyAccJerk-arCoeff()-X,1:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerk for X,1
* tBodyAccJerk-arCoeff()-X,2:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerk for X,2
* tBodyAccJerk-arCoeff()-X,3:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerk for X,3
* tBodyAccJerk-arCoeff()-X,4:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerk for X,4
* tBodyAccJerk-arCoeff()-Y,1:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerk for Y,1
* tBodyAccJerk-arCoeff()-Y,2:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerk for Y,2
* tBodyAccJerk-arCoeff()-Y,3:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerk for Y,3
* tBodyAccJerk-arCoeff()-Y,4:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerk for Y,4
* tBodyAccJerk-arCoeff()-Z,1:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerk for Z,1
* tBodyAccJerk-arCoeff()-Z,2:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerk for Z,2
* tBodyAccJerk-arCoeff()-Z,3:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerk for Z,3
* tBodyAccJerk-arCoeff()-Z,4:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerk for Z,4
* tBodyAccJerk-correlation()-X,Y:  correlation coefficient between two signals of tBodyAccJerk for X,Y
* tBodyAccJerk-correlation()-X,Z:  correlation coefficient between two signals of tBodyAccJerk for X,Z
* tBodyAccJerk-correlation()-Y,Z:  correlation coefficient between two signals of tBodyAccJerk for Y,Z
* tBodyGyro-mean()-X:  Mean value of tBodyGyro for X
* tBodyGyro-mean()-Y:  Mean value of tBodyGyro for Y
* tBodyGyro-mean()-Z:  Mean value of tBodyGyro for Z
* tBodyGyro-std()-X:  Standard deviation of tBodyGyro for X
* tBodyGyro-std()-Y:  Standard deviation of tBodyGyro for Y
* tBodyGyro-std()-Z:  Standard deviation of tBodyGyro for Z
* tBodyGyro-mad()-X:  Median absolute deviation  of tBodyGyro for X
* tBodyGyro-mad()-Y:  Median absolute deviation  of tBodyGyro for Y
* tBodyGyro-mad()-Z:  Median absolute deviation  of tBodyGyro for Z
* tBodyGyro-max()-X:  Largest value in array of tBodyGyro for X
* tBodyGyro-max()-Y:  Largest value in array of tBodyGyro for Y
* tBodyGyro-max()-Z:  Largest value in array of tBodyGyro for Z
* tBodyGyro-min()-X:  Smallest value in array of tBodyGyro for X
* tBodyGyro-min()-Y:  Smallest value in array of tBodyGyro for Y
* tBodyGyro-min()-Z:  Smallest value in array of tBodyGyro for Z
* tBodyGyro-sma():  Signal magnitude area of tBodyGyro 
* tBodyGyro-energy()-X:  Energy measure. Sum of the squares divided by the number of values.  of tBodyGyro for X
* tBodyGyro-energy()-Y:  Energy measure. Sum of the squares divided by the number of values.  of tBodyGyro for Y
* tBodyGyro-energy()-Z:  Energy measure. Sum of the squares divided by the number of values.  of tBodyGyro for Z
* tBodyGyro-iqr()-X:  Interquartile range of tBodyGyro for X
* tBodyGyro-iqr()-Y:  Interquartile range of tBodyGyro for Y
* tBodyGyro-iqr()-Z:  Interquartile range of tBodyGyro for Z
* tBodyGyro-entropy()-X:  Signal entropy of tBodyGyro for X
* tBodyGyro-entropy()-Y:  Signal entropy of tBodyGyro for Y
* tBodyGyro-entropy()-Z:  Signal entropy of tBodyGyro for Z
* tBodyGyro-arCoeff()-X,1:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyro for X,1
* tBodyGyro-arCoeff()-X,2:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyro for X,2
* tBodyGyro-arCoeff()-X,3:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyro for X,3
* tBodyGyro-arCoeff()-X,4:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyro for X,4
* tBodyGyro-arCoeff()-Y,1:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyro for Y,1
* tBodyGyro-arCoeff()-Y,2:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyro for Y,2
* tBodyGyro-arCoeff()-Y,3:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyro for Y,3
* tBodyGyro-arCoeff()-Y,4:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyro for Y,4
* tBodyGyro-arCoeff()-Z,1:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyro for Z,1
* tBodyGyro-arCoeff()-Z,2:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyro for Z,2
* tBodyGyro-arCoeff()-Z,3:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyro for Z,3
* tBodyGyro-arCoeff()-Z,4:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyro for Z,4
* tBodyGyro-correlation()-X,Y:  correlation coefficient between two signals of tBodyGyro for X,Y
* tBodyGyro-correlation()-X,Z:  correlation coefficient between two signals of tBodyGyro for X,Z
* tBodyGyro-correlation()-Y,Z:  correlation coefficient between two signals of tBodyGyro for Y,Z
* tBodyGyroJerk-mean()-X:  Mean value of tBodyGyroJerk for X
* tBodyGyroJerk-mean()-Y:  Mean value of tBodyGyroJerk for Y
* tBodyGyroJerk-mean()-Z:  Mean value of tBodyGyroJerk for Z
* tBodyGyroJerk-std()-X:  Standard deviation of tBodyGyroJerk for X
* tBodyGyroJerk-std()-Y:  Standard deviation of tBodyGyroJerk for Y
* tBodyGyroJerk-std()-Z:  Standard deviation of tBodyGyroJerk for Z
* tBodyGyroJerk-mad()-X:  Median absolute deviation of tBodyGyroJerk for X
* tBodyGyroJerk-mad()-Y:  Median absolute deviation of tBodyGyroJerk for Y
* tBodyGyroJerk-mad()-Z:  Median absolute deviation of tBodyGyroJerk for Z
* tBodyGyroJerk-max()-X:  Largest value in array of tBodyGyroJerk for X
* tBodyGyroJerk-max()-Y:  Largest value in array of tBodyGyroJerk for Y
* tBodyGyroJerk-max()-Z:  Largest value in array of tBodyGyroJerk for Z
* tBodyGyroJerk-min()-X:  Smallest value in array of tBodyGyroJerk for X
* tBodyGyroJerk-min()-Y:  Smallest value in array of tBodyGyroJerk for Y
* tBodyGyroJerk-min()-Z:  Smallest value in array of tBodyGyroJerk for Z
* tBodyGyroJerk-sma():  Signal magnitude area of tBodyGyroJerk 
* tBodyGyroJerk-energy()-X:  Energy measure. Sum of the squares divided by the number of values.  of tBodyGyroJerk for X
* tBodyGyroJerk-energy()-Y:  Energy measure. Sum of the squares divided by the number of values.  of tBodyGyroJerk for Y
* tBodyGyroJerk-energy()-Z:  Energy measure. Sum of the squares divided by the number of values.  of tBodyGyroJerk for Z
* tBodyGyroJerk-iqr()-X:  Interquartile range of tBodyGyroJerk for X
* tBodyGyroJerk-iqr()-Y:  Interquartile range of tBodyGyroJerk for Y
* tBodyGyroJerk-iqr()-Z:  Interquartile range of tBodyGyroJerk for Z
* tBodyGyroJerk-entropy()-X:  Signal entropy of tBodyGyroJerk for X
* tBodyGyroJerk-entropy()-Y:  Signal entropy of tBodyGyroJerk for Y
* tBodyGyroJerk-entropy()-Z:  Signal entropy of tBodyGyroJerk for Z
* tBodyGyroJerk-arCoeff()-X,1:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerk for X,1
* tBodyGyroJerk-arCoeff()-X,2:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerk for X,2
* tBodyGyroJerk-arCoeff()-X,3:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerk for X,3
* tBodyGyroJerk-arCoeff()-X,4:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerk for X,4
* tBodyGyroJerk-arCoeff()-Y,1:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerk for Y,1
* tBodyGyroJerk-arCoeff()-Y,2:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerk for Y,2
* tBodyGyroJerk-arCoeff()-Y,3:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerk for Y,3
* tBodyGyroJerk-arCoeff()-Y,4:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerk for Y,4
* tBodyGyroJerk-arCoeff()-Z,1:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerk for Z,1
* tBodyGyroJerk-arCoeff()-Z,2:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerk for Z,2
* tBodyGyroJerk-arCoeff()-Z,3:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerk for Z,3
* tBodyGyroJerk-arCoeff()-Z,4:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerk for Z,4
* tBodyGyroJerk-correlation()-X,Y:  correlation coefficient between two signals of tBodyGyroJerk for X,Y
* tBodyGyroJerk-correlation()-X,Z:  correlation coefficient between two signals of tBodyGyroJerk for X,Z
* tBodyGyroJerk-correlation()-Y,Z:  correlation coefficient between two signals of tBodyGyroJerk for Y,Z
* tBodyAccMag-mean():  Mean value of tBodyAccMag 
* tBodyAccMag-std():  Standard deviation of tBodyAccMag 
* tBodyAccMag-mad():  Median absolute deviation  of tBodyAccMag 
* tBodyAccMag-max():  Largest value in array of tBodyAccMag 
* tBodyAccMag-min():  Smallest value in array of tBodyAccMag 
* tBodyAccMag-sma():  Signal magnitude area of tBodyAccMag 
* tBodyAccMag-energy():  Energy measure. Sum of the squares divided by the number of values.  of tBodyAccMag 
* tBodyAccMag-iqr():  Interquartile range  of tBodyAccMag 
* tBodyAccMag-entropy():  Signal entropy of tBodyAccMag 
* tBodyAccMag-arCoeff()1:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccMag for 1
* tBodyAccMag-arCoeff()2:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccMag for 2
* tBodyAccMag-arCoeff()3:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccMag for 3
* tBodyAccMag-arCoeff()4:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccMag for 4
* tGravityAccMag-mean():  Mean value of tGravityAccMag 
* tGravityAccMag-std():  Standard deviation of tGravityAccMag 
* tGravityAccMag-mad():  Median absolute deviation  of tGravityAccMag 
* tGravityAccMag-max():  Largest value in array of tGravityAccMag 
* tGravityAccMag-min():  Smallest value in array of tGravityAccMag 
* tGravityAccMag-sma():  Signal magnitude area of tGravityAccMag 
* tGravityAccMag-energy():  Energy measure. Sum of the squares divided by the number of values.  of tGravityAccMag 
* tGravityAccMag-iqr():  Interquartile range  of tGravityAccMag 
* tGravityAccMag-entropy():  Signal entropy of tGravityAccMag 
* tGravityAccMag-arCoeff()1:  Autorregresion coefficients with Burg order equal to 4 of tGravityAccMag for 1
* tGravityAccMag-arCoeff()2:  Autorregresion coefficients with Burg order equal to 4 of tGravityAccMag for 2
* tGravityAccMag-arCoeff()3:  Autorregresion coefficients with Burg order equal to 4 of tGravityAccMag for 3
* tGravityAccMag-arCoeff()4:  Autorregresion coefficients with Burg order equal to 4 of tGravityAccMag for 4
* tBodyAccJerkMag-mean():  Mean value of tBodyAccJerkMag 
* tBodyAccJerkMag-std():  Standard deviation of tBodyAccJerkMag 
* tBodyAccJerkMag-mad():  Median absolute deviation  of tBodyAccJerkMag 
* tBodyAccJerkMag-max():  Largest value in array of tBodyAccJerkMag 
* tBodyAccJerkMag-min():  Smallest value in array of tBodyAccJerkMag 
* tBodyAccJerkMag-sma():  Signal magnitude area of tBodyAccJerkMag 
* tBodyAccJerkMag-energy():  Energy measure. Sum of the squares divided by the number of values.  of tBodyAccJerkMag 
* tBodyAccJerkMag-iqr():  Interquartile range  of tBodyAccJerkMag 
* tBodyAccJerkMag-entropy():  Signal entropy of tBodyAccJerkMag 
* tBodyAccJerkMag-arCoeff()1:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerkMag for 1
* tBodyAccJerkMag-arCoeff()2:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerkMag for 2
* tBodyAccJerkMag-arCoeff()3:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerkMag for 3
* tBodyAccJerkMag-arCoeff()4:  Autorregresion coefficients with Burg order equal to 4 of tBodyAccJerkMag for 4
* tBodyGyroMag-mean():  Mean value of tBodyGyroMag 
* tBodyGyroMag-std():  Standard deviation of tBodyGyroMag 
* tBodyGyroMag-mad():  Median absolute deviation  of tBodyGyroMag 
* tBodyGyroMag-max():  Largest value in array of tBodyGyroMag 
* tBodyGyroMag-min():  Smallest value in array of tBodyGyroMag 
* tBodyGyroMag-sma():  Signal magnitude area of tBodyGyroMag 
* tBodyGyroMag-energy():  Energy measure. Sum of the squares divided by the number of values.  of tBodyGyroMag 
* tBodyGyroMag-iqr():  Interquartile range  of tBodyGyroMag 
* tBodyGyroMag-entropy():  Signal entropy of tBodyGyroMag 
* tBodyGyroMag-arCoeff()1:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroMag for 1
* tBodyGyroMag-arCoeff()2:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroMag for 2
* tBodyGyroMag-arCoeff()3:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroMag for 3
* tBodyGyroMag-arCoeff()4:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroMag for 4
* tBodyGyroJerkMag-mean():  Mean value of tBodyGyroJerkMag 
* tBodyGyroJerkMag-std():  Standard deviation of tBodyGyroJerkMag 
* tBodyGyroJerkMag-mad():  Median absolute deviation  of tBodyGyroJerkMag 
* tBodyGyroJerkMag-max():  Largest value in array of tBodyGyroJerkMag 
* tBodyGyroJerkMag-min():  Smallest value in array of tBodyGyroJerkMag 
* tBodyGyroJerkMag-sma():  Signal magnitude area of tBodyGyroJerkMag 
* tBodyGyroJerkMag-energy():  Energy measure. Sum of the squares divided by the number of values.  of tBodyGyroJerkMag 
* tBodyGyroJerkMag-iqr():  Interquartile range  of tBodyGyroJerkMag 
* tBodyGyroJerkMag-entropy():  Signal entropy of tBodyGyroJerkMag 
* tBodyGyroJerkMag-arCoeff()1:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerkMag for 1
* tBodyGyroJerkMag-arCoeff()2:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerkMag for 2
* tBodyGyroJerkMag-arCoeff()3:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerkMag for 3
* tBodyGyroJerkMag-arCoeff()4:  Autorregresion coefficients with Burg order equal to 4 of tBodyGyroJerkMag for 4
* fBodyAcc-mean()-X:  Mean value of fBodyAcc for X
* fBodyAcc-mean()-Y:  Mean value of fBodyAcc for Y
* fBodyAcc-mean()-Z:  Mean value of fBodyAcc for Z
* fBodyAcc-std()-X:  Standard deviation of fBodyAcc for X
* fBodyAcc-std()-Y:  Standard deviation of fBodyAcc for Y
* fBodyAcc-std()-Z:  Standard deviation of fBodyAcc for Z
* fBodyAcc-mad()-X:  Median absolute deviation  of fBodyAcc for X
* fBodyAcc-mad()-Y:  Median absolute deviation  of fBodyAcc for Y
* fBodyAcc-mad()-Z:  Median absolute deviation  of fBodyAcc for Z
* fBodyAcc-max()-X:  Largest value in array of fBodyAcc for X
* fBodyAcc-max()-Y:  Largest value in array of fBodyAcc for Y
* fBodyAcc-max()-Z:  Largest value in array of fBodyAcc for Z
* fBodyAcc-min()-X:  Smallest value in array of fBodyAcc for X
* fBodyAcc-min()-Y:  Smallest value in array of fBodyAcc for Y
* fBodyAcc-min()-Z:  Smallest value in array of fBodyAcc for Z
* fBodyAcc-sma():  Signal magnitude area of fBodyAcc 
* fBodyAcc-energy()-X:  Energy measure. Sum of the squares divided by the number of values.  of fBodyAcc for X
* fBodyAcc-energy()-Y:  Energy measure. Sum of the squares divided by the number of values.  of fBodyAcc for Y
* fBodyAcc-energy()-Z:  Energy measure. Sum of the squares divided by the number of values.  of fBodyAcc for Z
* fBodyAcc-iqr()-X:  Interquartile range  of fBodyAcc for X
* fBodyAcc-iqr()-Y:  Interquartile range  of fBodyAcc for Y
* fBodyAcc-iqr()-Z:  Interquartile range  of fBodyAcc for Z
* fBodyAcc-entropy()-X:  Signal entropy of fBodyAcc for X
* fBodyAcc-entropy()-Y:  Signal entropy of fBodyAcc for Y
* fBodyAcc-entropy()-Z:  Signal entropy of fBodyAcc for Z
* fBodyAcc-maxInds-X:  index of the frequency component with largest magnitude of fBodyAcc for X
* fBodyAcc-maxInds-Y:  index of the frequency component with largest magnitude of fBodyAcc for Y
* fBodyAcc-maxInds-Z:  index of the frequency component with largest magnitude of fBodyAcc for Z
* fBodyAcc-meanFreq()-X:  Weighted average of the frequency components to obtain a mean frequency of fBodyAcc for X
* fBodyAcc-meanFreq()-Y:  Weighted average of the frequency components to obtain a mean frequency of fBodyAcc for Y
* fBodyAcc-meanFreq()-Z:  Weighted average of the frequency components to obtain a mean frequency of fBodyAcc for Z
* fBodyAcc-skewness()-X:  skewness of the frequency domain signal  of fBodyAcc for X
* fBodyAcc-kurtosis()-X:  kurtosis of the frequency domain signal  of fBodyAcc for X
* fBodyAcc-skewness()-Y:  skewness of the frequency domain signal  of fBodyAcc for Y
* fBodyAcc-kurtosis()-Y:  kurtosis of the frequency domain signal  of fBodyAcc for Y
* fBodyAcc-skewness()-Z:  skewness of the frequency domain signal  of fBodyAcc for Z
* fBodyAcc-kurtosis()-Z:  kurtosis of the frequency domain signal  of fBodyAcc for Z
* fBodyAcc-bandsEnergy()-1,8:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 1,8
* fBodyAcc-bandsEnergy()-9,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 9,16
* fBodyAcc-bandsEnergy()-17,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 17,24
* fBodyAcc-bandsEnergy()-25,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 25,32
* fBodyAcc-bandsEnergy()-33,40:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 33,40
* fBodyAcc-bandsEnergy()-41,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 41,48
* fBodyAcc-bandsEnergy()-49,56:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 49,56
* fBodyAcc-bandsEnergy()-57,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 57,64
* fBodyAcc-bandsEnergy()-1,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 1,16
* fBodyAcc-bandsEnergy()-17,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 17,32
* fBodyAcc-bandsEnergy()-33,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 33,48
* fBodyAcc-bandsEnergy()-49,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 49,64
* fBodyAcc-bandsEnergy()-1,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 1,24
* fBodyAcc-bandsEnergy()-25,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 25,48
* fBodyAcc-bandsEnergy()-1,8:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 1,8
* fBodyAcc-bandsEnergy()-9,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 9,16
* fBodyAcc-bandsEnergy()-17,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 17,24
* fBodyAcc-bandsEnergy()-25,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 25,32
* fBodyAcc-bandsEnergy()-33,40:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 33,40
* fBodyAcc-bandsEnergy()-41,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 41,48
* fBodyAcc-bandsEnergy()-49,56:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 49,56
* fBodyAcc-bandsEnergy()-57,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 57,64
* fBodyAcc-bandsEnergy()-1,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 1,16
* fBodyAcc-bandsEnergy()-17,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 17,32
* fBodyAcc-bandsEnergy()-33,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 33,48
* fBodyAcc-bandsEnergy()-49,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 49,64
* fBodyAcc-bandsEnergy()-1,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 1,24
* fBodyAcc-bandsEnergy()-25,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 25,48
* fBodyAcc-bandsEnergy()-1,8:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 1,8
* fBodyAcc-bandsEnergy()-9,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 9,16
* fBodyAcc-bandsEnergy()-17,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 17,24
* fBodyAcc-bandsEnergy()-25,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 25,32
* fBodyAcc-bandsEnergy()-33,40:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 33,40
* fBodyAcc-bandsEnergy()-41,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 41,48
* fBodyAcc-bandsEnergy()-49,56:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 49,56
* fBodyAcc-bandsEnergy()-57,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 57,64
* fBodyAcc-bandsEnergy()-1,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 1,16
* fBodyAcc-bandsEnergy()-17,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 17,32
* fBodyAcc-bandsEnergy()-33,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 33,48
* fBodyAcc-bandsEnergy()-49,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 49,64
* fBodyAcc-bandsEnergy()-1,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 1,24
* fBodyAcc-bandsEnergy()-25,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAcc for 25,48
* fBodyAccJerk-mean()-X:  Mean value of fBodyAccJerk for X
* fBodyAccJerk-mean()-Y:  Mean value of fBodyAccJerk for Y
* fBodyAccJerk-mean()-Z:  Mean value of fBodyAccJerk for Z
* fBodyAccJerk-std()-X:  Standard deviation of fBodyAccJerk for X
* fBodyAccJerk-std()-Y:  Standard deviation of fBodyAccJerk for Y
* fBodyAccJerk-std()-Z:  Standard deviation of fBodyAccJerk for Z
* fBodyAccJerk-mad()-X:  Median absolute deviation  of fBodyAccJerk for X
* fBodyAccJerk-mad()-Y:  Median absolute deviation  of fBodyAccJerk for Y
* fBodyAccJerk-mad()-Z:  Median absolute deviation  of fBodyAccJerk for Z
* fBodyAccJerk-max()-X:  Largest value in array of fBodyAccJerk for X
* fBodyAccJerk-max()-Y:  Largest value in array of fBodyAccJerk for Y
* fBodyAccJerk-max()-Z:  Largest value in array of fBodyAccJerk for Z
* fBodyAccJerk-min()-X:  Smallest value in array of fBodyAccJerk for X
* fBodyAccJerk-min()-Y:  Smallest value in array of fBodyAccJerk for Y
* fBodyAccJerk-min()-Z:  Smallest value in array of fBodyAccJerk for Z
* fBodyAccJerk-sma():  Signal magnitude area of fBodyAccJerk 
* fBodyAccJerk-energy()-X:  Energy measure. Sum of the squares divided by the number of values.  of fBodyAccJerk for X
* fBodyAccJerk-energy()-Y:  Energy measure. Sum of the squares divided by the number of values.  of fBodyAccJerk for Y
* fBodyAccJerk-energy()-Z:  Energy measure. Sum of the squares divided by the number of values.  of fBodyAccJerk for Z
* fBodyAccJerk-iqr()-X:  Interquartile range  of fBodyAccJerk for X
* fBodyAccJerk-iqr()-Y:  Interquartile range  of fBodyAccJerk for Y
* fBodyAccJerk-iqr()-Z:  Interquartile range  of fBodyAccJerk for Z
* fBodyAccJerk-entropy()-X:  Signal entropy of fBodyAccJerk for X
* fBodyAccJerk-entropy()-Y:  Signal entropy of fBodyAccJerk for Y
* fBodyAccJerk-entropy()-Z:  Signal entropy of fBodyAccJerk for Z
* fBodyAccJerk-maxInds-X:  index of the frequency component with largest magnitude of fBodyAccJerk for X
* fBodyAccJerk-maxInds-Y:  index of the frequency component with largest magnitude of fBodyAccJerk for Y
* fBodyAccJerk-maxInds-Z:  index of the frequency component with largest magnitude of fBodyAccJerk for Z
* fBodyAccJerk-meanFreq()-X:  Weighted average of the frequency components to obtain a mean frequency of fBodyAccJerk for X
* fBodyAccJerk-meanFreq()-Y:  Weighted average of the frequency components to obtain a mean frequency of fBodyAccJerk for Y
* fBodyAccJerk-meanFreq()-Z:  Weighted average of the frequency components to obtain a mean frequency of fBodyAccJerk for Z
* fBodyAccJerk-skewness()-X:  skewness of the frequency domain signal  of fBodyAccJerk for X
* fBodyAccJerk-kurtosis()-X:  kurtosis of the frequency domain signal  of fBodyAccJerk for X
* fBodyAccJerk-skewness()-Y:  skewness of the frequency domain signal  of fBodyAccJerk for Y
* fBodyAccJerk-kurtosis()-Y:  kurtosis of the frequency domain signal  of fBodyAccJerk for Y
* fBodyAccJerk-skewness()-Z:  skewness of the frequency domain signal  of fBodyAccJerk for Z
* fBodyAccJerk-kurtosis()-Z:  kurtosis of the frequency domain signal  of fBodyAccJerk for Z
* fBodyAccJerk-bandsEnergy()-1,8:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 1,8
* fBodyAccJerk-bandsEnergy()-9,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 9,16
* fBodyAccJerk-bandsEnergy()-17,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 17,24
* fBodyAccJerk-bandsEnergy()-25,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 25,32
* fBodyAccJerk-bandsEnergy()-33,40:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 33,40
* fBodyAccJerk-bandsEnergy()-41,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 41,48
* fBodyAccJerk-bandsEnergy()-49,56:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 49,56
* fBodyAccJerk-bandsEnergy()-57,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 57,64
* fBodyAccJerk-bandsEnergy()-1,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 1,16
* fBodyAccJerk-bandsEnergy()-17,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 17,32
* fBodyAccJerk-bandsEnergy()-33,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 33,48
* fBodyAccJerk-bandsEnergy()-49,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 49,64
* fBodyAccJerk-bandsEnergy()-1,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 1,24
* fBodyAccJerk-bandsEnergy()-25,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 25,48
* fBodyAccJerk-bandsEnergy()-1,8:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 1,8
* fBodyAccJerk-bandsEnergy()-9,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 9,16
* fBodyAccJerk-bandsEnergy()-17,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 17,24
* fBodyAccJerk-bandsEnergy()-25,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 25,32
* fBodyAccJerk-bandsEnergy()-33,40:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 33,40
* fBodyAccJerk-bandsEnergy()-41,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 41,48
* fBodyAccJerk-bandsEnergy()-49,56:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 49,56
* fBodyAccJerk-bandsEnergy()-57,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 57,64
* fBodyAccJerk-bandsEnergy()-1,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 1,16
* fBodyAccJerk-bandsEnergy()-17,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 17,32
* fBodyAccJerk-bandsEnergy()-33,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 33,48
* fBodyAccJerk-bandsEnergy()-49,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 49,64
* fBodyAccJerk-bandsEnergy()-1,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 1,24
* fBodyAccJerk-bandsEnergy()-25,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 25,48
* fBodyAccJerk-bandsEnergy()-1,8:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 1,8
* fBodyAccJerk-bandsEnergy()-9,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 9,16
* fBodyAccJerk-bandsEnergy()-17,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 17,24
* fBodyAccJerk-bandsEnergy()-25,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 25,32
* fBodyAccJerk-bandsEnergy()-33,40:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 33,40
* fBodyAccJerk-bandsEnergy()-41,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 41,48
* fBodyAccJerk-bandsEnergy()-49,56:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 49,56
* fBodyAccJerk-bandsEnergy()-57,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 57,64
* fBodyAccJerk-bandsEnergy()-1,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 1,16
* fBodyAccJerk-bandsEnergy()-17,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 17,32
* fBodyAccJerk-bandsEnergy()-33,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 33,48
* fBodyAccJerk-bandsEnergy()-49,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 49,64
* fBodyAccJerk-bandsEnergy()-1,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 1,24
* fBodyAccJerk-bandsEnergy()-25,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyAccJerk for 25,48
* fBodyGyro-mean()-X:  Mean value of fBodyGyro for X
* fBodyGyro-mean()-Y:  Mean value of fBodyGyro for Y
* fBodyGyro-mean()-Z:  Mean value of fBodyGyro for Z
* fBodyGyro-std()-X:  Standard deviation of fBodyGyro for X
* fBodyGyro-std()-Y:  Standard deviation of fBodyGyro for Y
* fBodyGyro-std()-Z:  Standard deviation of fBodyGyro for Z
* fBodyGyro-mad()-X:  Median absolute deviation  of fBodyGyro for X
* fBodyGyro-mad()-Y:  Median absolute deviation  of fBodyGyro for Y
* fBodyGyro-mad()-Z:  Median absolute deviation  of fBodyGyro for Z
* fBodyGyro-max()-X:  Largest value in array of fBodyGyro for X
* fBodyGyro-max()-Y:  Largest value in array of fBodyGyro for Y
* fBodyGyro-max()-Z:  Largest value in array of fBodyGyro for Z
* fBodyGyro-min()-X:  Smallest value in array of fBodyGyro for X
* fBodyGyro-min()-Y:  Smallest value in array of fBodyGyro for Y
* fBodyGyro-min()-Z:  Smallest value in array of fBodyGyro for Z
* fBodyGyro-sma():  Signal magnitude area of fBodyGyro 
* fBodyGyro-energy()-X:  Energy measure. Sum of the squares divided by the number of values.  of fBodyGyro for X
* fBodyGyro-energy()-Y:  Energy measure. Sum of the squares divided by the number of values.  of fBodyGyro for Y
* fBodyGyro-energy()-Z:  Energy measure. Sum of the squares divided by the number of values.  of fBodyGyro for Z
* fBodyGyro-iqr()-X:  Interquartile range  of fBodyGyro for X
* fBodyGyro-iqr()-Y:  Interquartile range  of fBodyGyro for Y
* fBodyGyro-iqr()-Z:  Interquartile range  of fBodyGyro for Z
* fBodyGyro-entropy()-X:  Signal entropy of fBodyGyro for X
* fBodyGyro-entropy()-Y:  Signal entropy of fBodyGyro for Y
* fBodyGyro-entropy()-Z:  Signal entropy of fBodyGyro for Z
* fBodyGyro-maxInds-X:  index of the frequency component with largest magnitude of fBodyGyro for X
* fBodyGyro-maxInds-Y:  index of the frequency component with largest magnitude of fBodyGyro for Y
* fBodyGyro-maxInds-Z:  index of the frequency component with largest magnitude of fBodyGyro for Z
* fBodyGyro-meanFreq()-X:  Weighted average of the frequency components to obtain a mean frequency of fBodyGyro for X
* fBodyGyro-meanFreq()-Y:  Weighted average of the frequency components to obtain a mean frequency of fBodyGyro for Y
* fBodyGyro-meanFreq()-Z:  Weighted average of the frequency components to obtain a mean frequency of fBodyGyro for Z
* fBodyGyro-skewness()-X:  skewness of the frequency domain signal  of fBodyGyro for X
* fBodyGyro-kurtosis()-X:  kurtosis of the frequency domain signal  of fBodyGyro for X
* fBodyGyro-skewness()-Y:  skewness of the frequency domain signal  of fBodyGyro for Y
* fBodyGyro-kurtosis()-Y:  kurtosis of the frequency domain signal  of fBodyGyro for Y
* fBodyGyro-skewness()-Z:  skewness of the frequency domain signal  of fBodyGyro for Z
* fBodyGyro-kurtosis()-Z:  kurtosis of the frequency domain signal  of fBodyGyro for Z
* fBodyGyro-bandsEnergy()-1,8:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 1,8
* fBodyGyro-bandsEnergy()-9,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 9,16
* fBodyGyro-bandsEnergy()-17,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 17,24
* fBodyGyro-bandsEnergy()-25,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 25,32
* fBodyGyro-bandsEnergy()-33,40:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 33,40
* fBodyGyro-bandsEnergy()-41,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 41,48
* fBodyGyro-bandsEnergy()-49,56:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 49,56
* fBodyGyro-bandsEnergy()-57,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 57,64
* fBodyGyro-bandsEnergy()-1,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 1,16
* fBodyGyro-bandsEnergy()-17,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 17,32
* fBodyGyro-bandsEnergy()-33,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 33,48
* fBodyGyro-bandsEnergy()-49,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 49,64
* fBodyGyro-bandsEnergy()-1,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 1,24
* fBodyGyro-bandsEnergy()-25,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 25,48
* fBodyGyro-bandsEnergy()-1,8:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 1,8
* fBodyGyro-bandsEnergy()-9,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 9,16
* fBodyGyro-bandsEnergy()-17,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 17,24
* fBodyGyro-bandsEnergy()-25,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 25,32
* fBodyGyro-bandsEnergy()-33,40:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 33,40
* fBodyGyro-bandsEnergy()-41,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 41,48
* fBodyGyro-bandsEnergy()-49,56:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 49,56
* fBodyGyro-bandsEnergy()-57,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 57,64
* fBodyGyro-bandsEnergy()-1,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 1,16
* fBodyGyro-bandsEnergy()-17,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 17,32
* fBodyGyro-bandsEnergy()-33,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 33,48
* fBodyGyro-bandsEnergy()-49,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 49,64
* fBodyGyro-bandsEnergy()-1,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 1,24
* fBodyGyro-bandsEnergy()-25,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 25,48
* fBodyGyro-bandsEnergy()-1,8:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 1,8
* fBodyGyro-bandsEnergy()-9,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 9,16
* fBodyGyro-bandsEnergy()-17,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 17,24
* fBodyGyro-bandsEnergy()-25,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 25,32
* fBodyGyro-bandsEnergy()-33,40:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 33,40
* fBodyGyro-bandsEnergy()-41,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 41,48
* fBodyGyro-bandsEnergy()-49,56:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 49,56
* fBodyGyro-bandsEnergy()-57,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 57,64
* fBodyGyro-bandsEnergy()-1,16:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 1,16
* fBodyGyro-bandsEnergy()-17,32:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 17,32
* fBodyGyro-bandsEnergy()-33,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 33,48
* fBodyGyro-bandsEnergy()-49,64:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 49,64
* fBodyGyro-bandsEnergy()-1,24:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 1,24
* fBodyGyro-bandsEnergy()-25,48:  Energy of a frequency interval within the 64 bins of the FFT of each window. of fBodyGyro for 25,48
* fBodyAccMag-mean():  Mean value of fBodyAccMag 
* fBodyAccMag-std():  Standard deviation of fBodyAccMag 
* fBodyAccMag-mad():  Median absolute deviation  of fBodyAccMag 
* fBodyAccMag-max():  Largest value in array of fBodyAccMag 
* fBodyAccMag-min():  Smallest value in array of fBodyAccMag 
* fBodyAccMag-sma():  Signal magnitude area of fBodyAccMag 
* fBodyAccMag-energy():  Energy measure. Sum of the squares divided by the number of values.  of fBodyAccMag 
* fBodyAccMag-iqr():  Interquartile range  of fBodyAccMag 
* fBodyAccMag-entropy():  Signal entropy of fBodyAccMag 
* fBodyAccMag-maxInds:  index of the frequency component with largest magnitude of fBodyAccMag 
* fBodyAccMag-meanFreq():  Weighted average of the frequency components to obtain a mean frequency of fBodyAccMag 
* fBodyAccMag-skewness():  skewness of the frequency domain signal  of fBodyAccMag 
* fBodyAccMag-kurtosis():  kurtosis of the frequency domain signal  of fBodyAccMag 
* fBodyBodyAccJerkMag-mean():  Mean value of fBodyBodyAccJerkMag 
* fBodyBodyAccJerkMag-std():  Standard deviation of fBodyBodyAccJerkMag 
* fBodyBodyAccJerkMag-mad():  Median absolute deviation  of fBodyBodyAccJerkMag 
* fBodyBodyAccJerkMag-max():  Largest value in array of fBodyBodyAccJerkMag 
* fBodyBodyAccJerkMag-min():  Smallest value in array of fBodyBodyAccJerkMag 
* fBodyBodyAccJerkMag-sma():  Signal magnitude area of fBodyBodyAccJerkMag 
* fBodyBodyAccJerkMag-energy():  Energy measure. Sum of the squares divided by the number of values.  of fBodyBodyAccJerkMag 
* fBodyBodyAccJerkMag-iqr():  Interquartile range  of fBodyBodyAccJerkMag 
* fBodyBodyAccJerkMag-entropy():  Signal entropy of fBodyBodyAccJerkMag 
* fBodyBodyAccJerkMag-maxInds:  index of the frequency component with largest magnitude of fBodyBodyAccJerkMag 
* fBodyBodyAccJerkMag-meanFreq():  Weighted average of the frequency components to obtain a mean frequency of fBodyBodyAccJerkMag 
* fBodyBodyAccJerkMag-skewness():  skewness of the frequency domain signal  of fBodyBodyAccJerkMag 
* fBodyBodyAccJerkMag-kurtosis():  kurtosis of the frequency domain signal  of fBodyBodyAccJerkMag 
* fBodyBodyGyroMag-mean():  Mean value of fBodyBodyGyroMag 
* fBodyBodyGyroMag-std():  Standard deviation of fBodyBodyGyroMag 
* fBodyBodyGyroMag-mad():  Median absolute deviation  of fBodyBodyGyroMag 
* fBodyBodyGyroMag-max():  Largest value in array of fBodyBodyGyroMag 
* fBodyBodyGyroMag-min():  Smallest value in array of fBodyBodyGyroMag 
* fBodyBodyGyroMag-sma():  Signal magnitude area of fBodyBodyGyroMag 
* fBodyBodyGyroMag-energy():  Energy measure. Sum of the squares divided by the number of values.  of fBodyBodyGyroMag 
* fBodyBodyGyroMag-iqr():  Interquartile range  of fBodyBodyGyroMag 
* fBodyBodyGyroMag-entropy():  Signal entropy of fBodyBodyGyroMag 
* fBodyBodyGyroMag-maxInds:  index of the frequency component with largest magnitude of fBodyBodyGyroMag 
* fBodyBodyGyroMag-meanFreq():  Weighted average of the frequency components to obtain a mean frequency of fBodyBodyGyroMag 
* fBodyBodyGyroMag-skewness():  skewness of the frequency domain signal  of fBodyBodyGyroMag 
* fBodyBodyGyroMag-kurtosis():  kurtosis of the frequency domain signal  of fBodyBodyGyroMag 
* fBodyBodyGyroJerkMag-mean():  Mean value of fBodyBodyGyroJerkMag 
* fBodyBodyGyroJerkMag-std():  Standard deviation of fBodyBodyGyroJerkMag 
* fBodyBodyGyroJerkMag-mad():  Median absolute deviation  of fBodyBodyGyroJerkMag 
* fBodyBodyGyroJerkMag-max():  Largest value in array of fBodyBodyGyroJerkMag 
* fBodyBodyGyroJerkMag-min():  Smallest value in array of fBodyBodyGyroJerkMag 
* fBodyBodyGyroJerkMag-sma():  Signal magnitude area of fBodyBodyGyroJerkMag 
* fBodyBodyGyroJerkMag-energy():  Energy measure. Sum of the squares divided by the number of values.  of fBodyBodyGyroJerkMag 
* fBodyBodyGyroJerkMag-iqr():  Interquartile range  of fBodyBodyGyroJerkMag 
* fBodyBodyGyroJerkMag-entropy():  Signal entropy of fBodyBodyGyroJerkMag 
* fBodyBodyGyroJerkMag-maxInds:  index of the frequency component with largest magnitude of fBodyBodyGyroJerkMag 
* fBodyBodyGyroJerkMag-meanFreq():  Weighted average of the frequency components to obtain a mean frequency of fBodyBodyGyroJerkMag 
* fBodyBodyGyroJerkMag-skewness():  skewness of the frequency domain signal  of fBodyBodyGyroJerkMag 
* fBodyBodyGyroJerkMag-kurtosis():  kurtosis of the frequency domain signal  of fBodyBodyGyroJerkMag 
* angle(tBodyAccMean,gravity): angle(tBodyAccMean,gravity)
* angle(tBodyAccJerkMean),gravityMean): angle(tBodyAccJerkMean),gravityMean)
* angle(tBodyGyroMean,gravityMean): angle(tBodyGyroMean,gravityMean)
* angle(tBodyGyroJerkMean,gravityMean): angle(tBodyGyroJerkMean,gravityMean)
* angle(X,gravityMean): angle(X,gravityMean)
* angle(Y,gravityMean): angle(Y,gravityMean)
* angle(Z,gravityMean): angle(Z,gravityMean)