run_analysis <- function() {

        library(data.table)
        library(reshape2)
        
        setwd("UCI HAR Dataset")
        
# STEP 1 - Merges the training and the test sets to create one data set.
   
        #Extract and bind data from train set
        setwd("train")

        subject <- read.table("subject_train.txt")
        y <- read.table("y_train.txt")
        x <- read.table("X_train.txt")

        dataset_train <- cbind(subject, y, x)

        setwd("../")

        #Extract and bind data from test set
        setwd("test")
        
        subject <- read.table("subject_test.txt")
        y <- read.table("y_test.txt")
        x <- read.table("X_test.txt")

        dataset_test <- cbind(subject, y, x)

        #Bind test and train sets and label the first 2 columns
        dataset <- rbind(dataset_train,dataset_test) 

        names(dataset)[1:2] <- c("subjectid", "activityid")

# STEP 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
                
        setwd("../")

        count <- ncol(dataset)-2
        
        # Checks each feature label to see if it contains the word "mean" or "std" using a for loop
        # Aggregates the value into a "temp" variable 
        # Creates a "position" vector which indicates the position of TRUE values

        for (i in 1:count){
                
                temp_mean <- grepl("mean", as.character(read.table("features.txt")[i,2]))
                temp_std <- grepl("std", as.character(read.table("features.txt")[i,2]))
                temp <- FALSE
                
                if (temp_mean == FALSE) {
                        
                        if (temp_std == TRUE) {
                                temp <- temp_std
                        } #Else "temp" variable keeps the value FALSE
                } else {
                        temp <- TRUE
                }
                
                if (!exists("position")) {
                        position <- data.frame(temp)
                } else {
                        position <- rbind(position,temp)
                }  
        }
        
        position <- which(position[,1]) # Create a vector indicating the position of TRUE values
        position_shift <- position + 2 # Shift to take into account the first two columns of "dataset" dataframe
        
        # Creates a rearangged data set that extracts the column refering to "mean" and "standard deviation" values
        subdataset <- cbind(dataset[,1:2],dataset[,position_shift])

# STEP 3 - Uses descriptive activity names to name the activities in the data set

        activitynames <- read.table("activity_labels.txt")
        
        # Merge the activity names with the subdataset dataframe based on activity id
        subdataset <- merge(subdataset,activitynames, by.x="activityid", by.y="V1") #82 columns
        
        #Rearrange the columns to replace the activity id by activity names
        subdataset <- cbind(subdataset[,2],subdataset[,82],subdataset[3:81]) #81 columns

        #Change type of column 2 from factor to character
        subdataset[,2] <- as.character(subdataset[,2]) 

# STEP 4 - Appropriately labels the data set with descriptive variable names. 

        names(subdataset)[1:2] <- c("subjectid", "activity")
        
        #Extract names from the "features.txt" file
        lab_names <- read.table("features.txt")
        
        #extract the names according to the position variable obtained in step 2
        lab_names <- as.character(lab_names[position,2]) 

        # Apply the names to the subdataset and convert them to lower case parenthesis, extract parenthesis
        # and replace dots and minus signe by underscores
        names(subdataset)[3:81] <- lab_names
        names(subdataset) <- tolower(names(subdataset))
        names(subdataset) <- gsub("\\(\\)", "", names(subdataset))
        names(subdataset) <- gsub("\\-", "\\_", names(subdataset))
        names(subdataset) <- gsub("\\.", "\\_ ", names(subdataset))
        
# STEP 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        
        # Aggregates values by activity and subject ID and calculating means for each activity and each subject
        tidydata <- aggregate(subdataset[3:81],list(subdataset$activity, subdataset$subjectid), mean)
        names(tidydata)[1:2] <- c("activity","subjectid")

        #Exports the new dataset into a "tidy_data.txt" file
        write.table(tidydata, file = "tidy_data.txt")
        
        setwd("../")
}
