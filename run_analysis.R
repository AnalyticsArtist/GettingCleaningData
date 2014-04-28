## Program     : Coursera Data Science - Getting & Cleaning Data Peer Assessment
## Written By  : Gabriel Mohanna
## Date Created: Apr 27, 2014
## Narrative   : The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
##               The goal is to prepare tidy data that can be used for later analysis.
## Background  : One of the most exciting areas in all of data science right now is wearable computing - see for example 
##               this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced 
##               algorithms to attract new users. The data linked to from the course website represent data collected 
##               from the accelerometers from the Samsung Galaxy S smartphone.
## TBD         : 
##
## \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Code is Poetry >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
##
## **********************************************************************************************************************
## Steps
## -----
## (0) Load Libraries & Define Data Path
## (1) Merge the Training and the Test Sets to Create One Data Set
## (2) Extract Only the Measurements on the Mean and Standard Deviation for Each Measurement
## (3) Use Descriptive Activity Names to Name the Activities in the Data Set
## (4) Appropriately Label the Data Set with Descriptive Activity Names
## (5) Create a Second, Independent Tidy Data Set with the Average of Each Variable for Each Activity and Each Subject
##
## **********************************************************************************************************************
## Notes
## ---------------------
##
## **********************************************************************************************************************

# ***********************************************************************************************************************
# (0) Load Libraries & Define Data Path
# ***********************************************************************************************************************
# Load Libraries
library(data.table)
library(reshape2)
library(plyr)

# Define Data Path
data_path <- "/Users/gabrielm/OneDrive/Documents/HW/Coursera/Getting and Cleaning Data/Peer Assessment/UCI HAR Dataset"

setwd(data_path)

# End Load Libraries & Define Data Path


# ***********************************************************************************************************************
# (1) Merge the Training and the Test Sets to Create One Data Set
# ***********************************************************************************************************************
# Read Data
train_Subjects <- read.table("train/subject_train.txt")
train_Y        <- read.table("train/Y_train.txt")
train_X        <- read.table("train/X_train.txt")

test_Subjects  <- read.table("test/subject_test.txt")
test_Y         <- read.table("test/Y_test.txt")
test_X         <- read.table("test/X_test.txt")

# Read Features
features       <- read.table("features.txt")

# Read Activity Labels
activities     <- read.table("activity_labels.txt")

# Set Variable Names
# Please note: This is Q3 of the project.
names(train_Subjects) <- "subject_id"
names(train_Y)        <- "activity_cd"
names(train_X)        <- features[,2]

names(test_Subjects)  <- "subject_id"
names(test_Y)         <- "activity_cd"
names(test_X)         <- features[,2]

names(activities)     <- c("activity_cd", "activity")

# Merge All Frames Into Their Respective Frames
train_set <- cbind(train_Subjects, train_Y, train_X)
test_set  <- cbind(test_Subjects , test_Y , test_X )

# Merge the Training and the Test Sets to Create One Data Set
UCI_HAR_Dataset <- rbind(train_set, test_set)

# Print Final Data Size & Dimensions
print(object.size(UCI_HAR_Dataset), units="Mb")
dim(UCI_HAR_Dataset)

# Remove Unused Data Frames to Conserve Memory
rm(train_Subjects, train_Y, train_X, train_set,
   test_Subjects , test_Y , test_X , test_set  )

# End Read & Merge the Training and the Test Sets to Create One Data Set


# ***********************************************************************************************************************
# (2) Extract Only the Measurements on the Mean and Standard Deviation for Each Measurement
# ***********************************************************************************************************************
# I use the grepl command to find the locations within the features names that contain mean() or std() as well as the 
# subject & the activity
Selected_Measurements <- UCI_HAR_Dataset[,grepl("subject_id|activity_cd|[Mm][Ee][Aa][Nn]\\(\\)|[Ss][Tt][Dd]\\(\\)",
                                                names(UCI_HAR_Dataset))]


# End Extract Only the Measurements on the Mean and Standard Deviation for Each Measurement


# ***********************************************************************************************************************
# (3) Use Descriptive Activity Names to Name the Activities in the Data Set
# ***********************************************************************************************************************
# This step is already executed by naming the variables in step 1

# End Use Descriptive Activity Names to Name the Activities in the Data Set


# ***********************************************************************************************************************
# (4) Appropriately Label the Data Set with Descriptive Activity Names
# ***********************************************************************************************************************
UCI_HAR_Dataset <- merge(UCI_HAR_Dataset[,c(1:ncol(UCI_HAR_Dataset))], activities, by="activity_cd")
UCI_HAR_Dataset <- UCI_HAR_Dataset[, c(2, 1, ncol(UCI_HAR_Dataset), 3:(ncol(UCI_HAR_Dataset)-1))]

# End Appropriately Label the Data Set with Descriptive Activity Names


# ***********************************************************************************************************************
# (5) Create a Second, Independent Tidy Data Set with the Average of Each Variable for Each Activity and Each Subject
# ***********************************************************************************************************************
# Use the "by" function in combination with "colMean: function. Remove the activity column since it's not numeric.
# Save the list output of the "by" function as a data frame and set the names accordingly. Finally, remerge with 
# feature names and reorder columns.
tidyList <- by(data=UCI_HAR_Dataset[,-3],INDICES=UCI_HAR_Dataset[,c("subject_id", "activity_cd")], FUN=colMeans,simplify=T)
tidyData <- data.frame(matrix(unlist(tidyList), nrow=length(tidyList), byrow=T))
names(tidyData) <- names(UCI_HAR_Dataset[,-3])
tidyData <- merge(tidyData[,c(1:ncol(tidyData))], activities, by="activity_cd")
tidyData <- tidyData[, c(2, 1, ncol(tidyData), 3:(ncol(tidyData)-1))]

# End Create a Second, Independent Tidy Data Set with the Average of Each Variable for Each Activity and Each Subject
