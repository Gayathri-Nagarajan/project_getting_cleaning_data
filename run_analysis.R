#########################

# Title- run_analysis.R
#This file does the following

# GOAL- 1Merges the training and the test sets to create one data set.
#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement.
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with
#the average of each variable for each activity and each subject.




#######################
#1.Merges the training and the test sets to create one data set.
########################

#Lets first read the data from the files downloaded
#starting reference link -
#  https://www.coursera.org/learn/data-cleaning/discussions/weeks/4/threads/PPbxedoeEeeU5xKrF9OZPg
#########################
#read the training data
###########################
#Set the Working directory where data files exist
rm(list=ls())
setwd( "C:\\Users\\RamamurthyV\\Documents\\R\\3.Getting and Cleaning Data\\Project\\UCI HAR Dataset\\train")
library(dplyr)
#read x_train file, y_train into respective data table
x_train <- read.table("X_train.txt", header= FALSE)
#head(x_train,6) 
y_train <- read.table("Y_train.txt", header = FALSE)
#head(y_train,6)
#nrow(y_train) 
#nrow(x_train)
#rename the column to meaningful ones
names(y_train) <- "activity"


#read subject_train into a data table and rename column name
subject_train <- read.table("subject_train.txt", header = FALSE)
#head(subject_train)
#nrow(subject_train)
names(subject_train) <- "subject"
#cbind the x_train and subject_train


#Read the features data set from its directory
setwd( "C:\\Users\\RamamurthyV\\Documents\\R\\3.Getting and Cleaning Data\\Project\\UCI HAR Dataset")

features <- read.table("features.txt",header=FALSE)

#head(features,6)
#ncol(features)
#nrow(features)
#features$V2

#Now add features which is a column label to total_x
#rm("total_x") #remove this variable 
col_names <- features$V2



# Column bind x_train, y_train and subject_train data 
#rm("total_x")
names(x_train) <- col_names
#head(x_train)
 train_x_y <- cbind(x_train,y_train)
 train_x_y_sub <- cbind(train_x_y,subject_train)
# head(train_x_y_sub)
# head(train_x_y)
#ncol(train_x_y)
# nrow(x_train)
#ncol(x_train)
#read the features as label
#nrow(x_train)
#ncol(x_train)

#Final data is train_x_y_sub

#START HERE
#ref- https://stackoverflow.com/questions/24536771/conditionally-replacing-column-values-with-data-table

library(data.table)
library(plyr)


#class(train_x_y_sub$activity) #integer
#is.data.table(train_x_y_sub) #false
#class(train_x_y_sub) #DF
train_x_y_sub$activity <- as.character(train_x_y_sub$activity)
#class(train_x_y_sub$activity) #character



#replace the activity label with meaninful names 
train_x_y_sub$activity[train_x_y_sub$activity=="1"] <- "WALKING"
train_x_y_sub$activity[train_x_y_sub$activity=="2"] <- "WALKING_UPSTAIRS"
train_x_y_sub$activity[train_x_y_sub$activity=="3"] <-  "WALKING_DOWNSTAIRS"
train_x_y_sub$activity[train_x_y_sub$activity=="4"] <-  "SITTING"
train_x_y_sub$activity[train_x_y_sub$activity=="5"] <- "STANDING"
train_x_y_sub$activity[train_x_y_sub$activity=="6"] <- "LAYING"

#head(train_x_y_sub)




#read the test data

setwd( "C:\\Users\\RamamurthyV\\Documents\\R\\3.Getting and Cleaning Data\\Project\\UCI HAR Dataset\\test")
x_test <- read.table("X_test.txt", header= FALSE)
#head(x_test,6) 
#nrow(x_test)
names(x_test) <- col_names

y_test <- read.table("Y_test.txt", header = FALSE)
#head(y_test,6)
names(y_test) <- "activity"

#Read subject test data
subject_test <- read.table("subject_test.txt", header = FALSE)
#head(subject_test)
names(subject_test) <- "subject"


# column bind x_test,y_test,subject_test
test_x_y <- cbind(x_test,y_test)
test_x_y_sub <- cbind(test_x_y,subject_test)
#head(test_x_y_sub)
#head(test_x_y)
#ncol(test_x_y)
#nrow(x_test)
#ncol(x_test)


library(data.table)
library(plyr)


#class(test_x_y_sub$activity) #integer
#is.data.table(test_x_y_sub) #false
#class(test_x_y_sub) #DF
#Change the type of the column to character 
test_x_y_sub$activity <- as.character(test_x_y_sub$activity)
#class(test_x_y_sub$activity) #character



#replace the activity label with meaninful names 
test_x_y_sub$activity[test_x_y_sub$activity=="1"] <- "WALKING"
test_x_y_sub$activity[test_x_y_sub$activity=="2"] <- "WALKING_UPSTAIRS"
test_x_y_sub$activity[test_x_y_sub$activity=="3"] <-  "WALKING_DOWNSTAIRS"
test_x_y_sub$activity[test_x_y_sub$activity=="4"] <-  "SITTING"
test_x_y_sub$activity[test_x_y_sub$activity=="5"] <- "STANDING"
test_x_y_sub$activity[test_x_y_sub$activity=="6"] <- "LAYING"

#head(test_x_y_sub)



#merge training and test into 1 single dataset vertically using rbind as they have same # of columns 

total <- rbind(train_x_y_sub,test_x_y_sub)
#nrow(total)
#ncol(total)
#head(total)

# Extracts only the measurements on the mean and standard deviation for each measurement.
#use grepl to select only columns that have mean and std deviation
sum(grepl("mean",names(total))) #46
total_mean <- total[,grepl("mean",names(total))]
#head(total_mean)
#ncol(total_mean)#46


sum(grepl("std",names(total))) #33
total_std <- total[,grepl("std",names(total))]
#head(total_std)
#ncol(total_std)#33
#nrow(total_std)

#Merge total_mean and total_std by cbind 
total_ms <- cbind(total_mean,total_std)
#Add subject and activity
total_ms <- cbind(subject=total$subject,activity=total$activity,total_ms)
#head(total_ms)
#nrow(total_ms)# 10299
#ncol(total_ms)#81

names(total_ms)
#remove the  paranthesis () using gsub function from the column/variable names
names(total_ms)= gsub(pattern="[()]",replacement="",x=names(total_ms))

library(data.table)

#Make this data frame into a data table
total_ms_tbl <- tbl_df(total_ms)

#class(total_ms_tbl)
#remove the data frame
rm("total_ms")

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for
#each activity and each subject.
 
#names(total_ms_tbl)

#class(total_ms_tbl)
#81 columns



#Ref-        https://www.coursera.org/learn/data-cleaning/lecture/e5qVi/summarizing-data
        
        

library(dplyr)
#First group by subject, activity
#View(total_ms_tbl)
#head(total_ms_tbl)
# 6* 81 activity[1]
#81 columns
#nrow(total_ms_tbl) #10299
#ncol(total_ms_tbl) #81 columns 


#Get class of each column name in Data table
sapply(total_ms_tbl,class)

#Make subject column as character as we dont have to take its mean when summarising
total_ms_tbl$subject <- as.character(total_ms_tbl$subject)
sapply(total_ms_tbl,class)


#5 FINAL DATA to write into a file 
final_set <- total_ms_tbl %>% group_by(subject,activity)  %>% 
            summarise_if(is.numeric,mean)  %>% 
            arrange (subject,activity)


#head(final_set)
#ncol(final_set) #81
#nrow(final_set) #6
#View(final_set)
getwd()
setwd("C:\\Users\\RamamurthyV\\Documents\\R\\3.Getting and Cleaning Data\\Project")
library(xlsx)



#Write the final tidy wide data set to txt file
write.table(final_set, "final_clean_set.txt", row.names = FALSE, quote = FALSE)
View(final_set)
