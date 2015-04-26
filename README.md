Getting and Cleaning Data: Course Project
=========================================

Introduction
------------
This repository contains my work for the course project for the Coursera course "Getting and Cleaning data", part of the Data Science specialization.

About the raw data
------------------

The features (561 of them) are unlabeled and can be found in the x_test.txt. 
The activity labels are in the y_test.txt file.
The test subjects are in the subject_test.txt file.

The same holds for the training set.

About the script and the tidy dataset
-------------------------------------
I created a script called run_analysis.R which will merge the test and training sets together.
Prerequisites for this script:

1. the UCI HAR Dataset must be extracted and..
2. the UCI HAR Dataset must be availble in a directory called "UCI HAR Dataset"
3. you must set you active directory at the same path as the "UCI HAR Dataset" directory

After merging testing and training, labels are added and only columns that have to do with mean and standard deviation are kept.

Lastly, the script will create a tidy data set containing the means of all the columns per test subject and per activity.

This tidy dataset will be written to a tab-delimited file called tidy_data.txt, which can also be found in this repository.

About the Code Book
-------------------
The CodeBook.md file explains the transformations performed and the resulting data and variables.

