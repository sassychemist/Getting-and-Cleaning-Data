---
title: "Getting and Cleaning Data - Project"
output: github_document
---

This is the final project for Getting and Cleaning Data Coursera Class. This project runs an R Script called `run_analysis.R`, which does the following:
    
1. Downloads a file
2. Extracts the file contents into a directory that is created if it does not already exist
3. Loads activity and feature info
4. Loads training data and test datasets
5. Adds columns to indicate subject and activity associated with each observation of each dataset
6. Labels the variables using the feature info
7. Merges training and test data sets
8. Converts the `activity` and `subject` columns into factors
9. Subsets the Merged Data on Standard Deviation and Mean affiliated variables
10. Creates a copy of the first merged data set with a mean of each variable by each activity and subject
11. Writes the final copy of the data from Step 10 to a text file called `combinedData.txt`