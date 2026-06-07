
## This script checks that the dataset made in MATLAB and the original csv datasets in the shared folder are the same

#Import packages

#install.packages(dplyr)
library(dplyr)
#install.packages(tidyverse)
library(tidyverse)

#Create a dataframe from the shared participant csv files

# 1. List all CSV files in the directory
csv_files <- list.files(path = 'Path/to/main_directory/Data_experiment_1/CSV_individuals/', pattern = "*.csv", full.names = TRUE)

# 2. Read all files and bind them into one data frame
combined_data <- csv_files %>%
  map_dfr(read_csv)

# 3. Write the combined data to a single new CSV file
write_csv(combined_data, 'Path/to/main_directory/Data_experiment_1/CSV_individuals/data_orig.csv')

data_orig <- read.csv('Path/to/main_directory/Data_experiment_1/CSV_individuals/data_orig.csv')
backup_data_orig <- data_orig

#Read the csv file written from the MATLAB files 

data <- read.csv('Path/to/main_directory/Experiment_1_trial_data.csv')
backup_data <- data

#Get rid of extra columns in the datasets so that all that remains are what both have in common

data <- data[,c(4:8, 10)]
data_orig <- data_orig[,-c(4)]

#Add ID columns to both sets of data

new_vector <- c(1:135)

data <- data %>% mutate(ID = rep(new_vector, each = 540))


data_orig <- data_orig %>% mutate(ID = rep(new_vector, each = 540))

colnames(data) <- c("trial_num", "block", "trial_num_within_block", "set_size", "change", "rt", "id")
colnames(data_orig) <- c("trial_num", "trial_num_within_block", "block", "set_size", "change", "rt", "id")

#Reorder columns so they are the same

data <- data[, c("id", "trial_num", "trial_num_within_block", "block", "set_size", "change", "rt")]
data_orig <- data_orig[, c("id", "trial_num", "trial_num_within_block", "block", "set_size", "change", "rt")]

#Round off the RT variable

data$rt <- round(data$rt, 2)
data_orig$rt <- round(data_orig$rt, 2)

#Check to see if both datasets (the dataset compiled from the shared CSV files and the CSV file written to the main directory) are exactly the same. 

all.equal(data, data_orig) #If no other errors, then the dataframes are the same. Move forward from here to explore the data.

#Additionally check for the same number of rows and RT means/SDs

nrow(data)
nrow(data_orig) #The number of rows is the same in both dataframes

mean(data$rt)
sd(data$rt)

mean(data_orig$rt)
sd(data_orig$rt) #RT means and SDs are the same