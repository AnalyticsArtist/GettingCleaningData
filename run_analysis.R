## Program     : Coursera Data Science - Getting & Cleaning Data Peer Assessment
## Written By  : Gabriel Mohanna
## Date Created: Apr 27, 2014
## Background  : One of the most exciting areas in all of data science right now is wearable computing - see for example 
##               this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced 
##               algorithms to attract new users. The data linked to from the course website represent data collected 
##               from the accelerometers from the Samsung Galaxy S smartphone.
## Narrative   : The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
##               The goal is to prepare tidy data that can be used for later analysis.
## TBD         : 
##
## \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Code is Poetry >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## ----------------------------------------------------------------------------------------------------------------------
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

# Set Variable Names
names(train_Subjects) <- "subject"
names(train_Y)        <- "activity"
names(train_X)        <- features[,2]

names(test_Subjects)  <- "subject"
names(test_Y)         <- "activity"
names(test_X)         <- features[,2]

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
   test_Subjects , test_Y , test_X , test_set , features )

# End Read & Merge the Training and the Test Sets to Create One Data Set


# ***********************************************************************************************************************
# (2) Extract Only the Measurements on the Mean and Standard Deviation for Each Measurement
# ***********************************************************************************************************************


# End Extract Only the Measurements on the Mean and Standard Deviation for Each Measurement


# ***********************************************************************************************************************
# (3) Use Descriptive Activity Names to Name the Activities in the Data Set
# ***********************************************************************************************************************


# End Use Descriptive Activity Names to Name the Activities in the Data Set


# ***********************************************************************************************************************
# (4) Appropriately Label the Data Set with Descriptive Activity Names
# ***********************************************************************************************************************


# End Appropriately Label the Data Set with Descriptive Activity Names


# ***********************************************************************************************************************
# (5) Create a Second, Independent Tidy Data Set with the Average of Each Variable for Each Activity and Each Subject
# ***********************************************************************************************************************


# End Create a Second, Independent Tidy Data Set with the Average of Each Variable for Each Activity and Each Subject