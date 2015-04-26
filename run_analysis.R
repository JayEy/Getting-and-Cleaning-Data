run_analysis <- function() {
        
        directory <- "/Users/Joachim/Desktop/Coursera/Getting and Cleaning Data/Project/UCI HAR Dataset"
        #directorysignals <- 
        setwd(directory)
        
        library(data.table)
        library(reshape2)
        
# Merges the training and the test sets to create one data set.
   
        setwd("train")
        
        subject <- read.table("subject_train.txt")
        y <- read.table("y_train.txt")
        x <- read.table("X_train.txt")

        dataset_train <- cbind(subject, y, x) # 7352*563 data frame

        setwd(directory)
        setwd("test")
        
        subject <- read.table("subject_test.txt")
        y <- read.table("y_test.txt")
        x <- read.table("X_test.txt")

        dataset_test <- cbind(subject, y, x)

        dataset <- rbind(dataset_train,dataset_test) #10299 obs. of  563 variables:

        names(dataset)[1:2] <- c("subjectid", "labelid")

# Extracts only the measurements on the mean and standard deviation for each measurement. 
                
        setwd(directory)

        count <- ncol(dataset)-2

        for (i in 1:count){
                
                temp_mean <- grepl("mean", as.character(read.table("features.txt")[i,2]))
                temp_std <- grepl("std", as.character(read.table("features.txt")[i,2]))
                temp <- FALSE
                
                if (temp_mean == FALSE) {
                        
                        if (temp_std == TRUE) {
                                temp <- temp_std
                        }
                } else {
                        temp <- TRUE
                }
                
                if (!exists("position")) {
                        position <- data.frame(temp)
                } else {
                        position <- rbind(position,temp)
                }  
        }
        
        position <- which(position[,1])
        position_shift <- position + 2
        subdataset <- cbind(dataset[,1:2],dataset[,position_shift])

# Uses descriptive activity names to name the activities in the data set

        activitynames <- read.table("activity_labels.txt")

        subdataset <- merge(subdataset,activitynames, by.x="labelid", by.y="V1") #82 columns
        
        subdataset <- cbind(subdataset[,2],subdataset[,82],subdataset[3:81]) #81 columns

        subdataset[,2] <- as.character(subdataset[,2]) #change type of column 2 from factor to character

# Appropriately labels the data set with descriptive variable names. 

        names(subdataset)[1:2] <- c("subjectid", "activity")

        setwd(directory)

        lab_names <- read.table("features.txt")

        lab_names <- as.character(lab_names[position,2]) #extract the names according to the position variable obtain in step 2

        names(subdataset)[3:81] <- lab_names

        names(subdataset) <- tolower(names(subdataset))
        names(subdataset) <- gsub("\\(\\)", "", names(subdataset))
        names(subdataset) <- gsub("\\-", "\\_", names(subdataset))
        names(subdataset) <- gsub("\\.", "\\_ ", names(subdataset))
        
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        
        subdataset <- aggregate(subdataset[3:81],list(subdataset$activity, subdataset$subjectid), mean)
        names(subdataset)[1:2] <- c("activity","subjectid")

        write.table(subdataset, file = "tidy_data.txt")

}