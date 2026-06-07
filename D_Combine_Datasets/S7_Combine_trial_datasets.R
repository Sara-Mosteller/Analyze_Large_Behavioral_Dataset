###########################################################################################################
#This script combines the Expts 1 and 2 from dataset 1 with dataset 2, keeping overlapping columns. 
#Also reformats and saves the experiments in dataset 1 
###########################################################################################################

#Import the needed packages

#install.packages(dplyr)
library(dplyr)
#install.packages(tidyverse)
library(tidyverse)

#Read and store the datasets as dataframes

#Dataset 1, expt 1, (Xu et al., 2018, Expt 1)
dataset1_expt1 <- read.csv('Path/to/Experiment_1_trial_data.csv')

#Dataset 1, expt 2 (Xu et al., 2018, Expt 2)
dataset1_expt2 <- read.csv('Path/to/Experiment_2_trial_data.csv')

#Dataset 2 (Balaban et al., 2019)
dataset2 <- read.csv('Path/to/Trial_Data.csv')

###########################################################################################################
#Check the dataset properties
###########################################################################################################

#Check that the conditions are counterbalanced within blocks of the dataset 1

dataset1_expt1_counterbalancecheck <- dataset1_expt1 %>% group_by(id, block, change, set_size) %>% count() #Conditions are counterbalanced within blocks
dataset1_expt2_counterbalancecheck <- dataset1_expt2 %>% group_by(id, session, block, change, set_size) %>% count() #Conditions are counterbalanced within sessions and blocks

#Compare the mean, median, and midpoint of the x and y locations

#Start with dataset 1, expt 1
#Check the test column

mean(dataset1_expt1$test_location_x) #523
median(dataset1_expt1$test_location_x) #565
min(dataset1_expt1$test_location_x) #222
max(dataset1_expt1$test_location_x) #1016
#The midpoint is (1016-222)/2 <- 397

mean(dataset1_expt1$test_location_y) #394
median(dataset1_expt1$test_location_y) #437
min(dataset1_expt1$test_location_y) #179
max(dataset1_expt1$test_location_y) #803
#The midpoint is (803-179)/2 <- 312


#Check the array columns
colMeans(dataset1_expt1[, c(30:37)], na.rm <- TRUE) #location x mean is 521.5
colMeans(dataset1_expt1[, c(38:45)], na.rm <- TRUE) #location y mean is 410

dim(dataset1_expt1) #Should be 79200 rows and 45 columns

#Dataset 1, expt 2

#Check the array columns
colMeans(dataset1_expt2[, c(33:40)], na.rm <- TRUE) #location x mean is 681
colMeans(dataset1_expt2[, c(41:48)], na.rm <- TRUE) #location y mean is 425

dim(dataset1_expt2) #Should be 293880 rows and 48 columns

###########################################################################################################
#Format the datasets
###########################################################################################################

#Choose/format rows and columns for dataset1_expt1

#Add or standardize columns
dataset1_expt1$study <- 1 #Xu et al., 2018
dataset1_expt1$experiment <- 1 #Expt 1
dataset1_expt1$session <- 1 
dataset1_expt1$rt <-  dataset1_expt1$rt*1000
dataset1_expt1$distance_from_monitor <- 60 #ms
dataset1_expt1$stim_size <- 1.775 #average degrees of visual angle
dataset1_expt1$fixation_size <- 0.415 #average degrees of visual angle
dataset1_expt1$stim_duration <- 250 #ms
dataset1_expt1$intertrial_interval <- 1000 #ms
dataset1_expt1$retention_interval <- 1000 #ms
dataset1_expt1$break_duration <- 30000 #ms

#create the final location variables
dataset1_expt1$test_location_x_from_center <- dataset1_expt1$test_location_x - 522 #pixels from the mean
dataset1_expt1$test_location_y_from_center <- dataset1_expt1$test_location_y - 410 #pixels from the mean
dataset1_expt1$item_location_1_x_from_center <- dataset1_expt1$item_location_1_x - 522 #pixels from the mean
dataset1_expt1$item_location_1_y_from_center <- dataset1_expt1$item_location_1_y - 410 #pixels from the mean
dataset1_expt1$item_location_2_x_from_center <- dataset1_expt1$item_location_2_x - 522 #pixels from the mean
dataset1_expt1$item_location_2_y_from_center <- dataset1_expt1$item_location_2_y - 410 #pixels from the mean
dataset1_expt1$item_location_3_x_from_center <- dataset1_expt1$item_location_3_x - 522 #pixels from the mean
dataset1_expt1$item_location_3_y_from_center <- dataset1_expt1$item_location_3_y - 410 #pixels from the mean
dataset1_expt1$item_location_4_x_from_center <- dataset1_expt1$item_location_4_x - 522 #pixels from the mean
dataset1_expt1$item_location_4_y_from_center <- dataset1_expt1$item_location_4_y - 410 #pixels from the mean
dataset1_expt1$item_location_5_x_from_center <- dataset1_expt1$item_location_5_x - 522 #pixels from the mean
dataset1_expt1$item_location_5_y_from_center <- dataset1_expt1$item_location_5_y - 410 #pixels from the mean
dataset1_expt1$item_location_6_x_from_center <- dataset1_expt1$item_location_6_x - 522 #pixels from the mean
dataset1_expt1$item_location_6_y_from_center <- dataset1_expt1$item_location_6_y - 410 #pixels from the mean
dataset1_expt1$item_location_7_x_from_center <- dataset1_expt1$item_location_7_x - 522 #pixels from the mean
dataset1_expt1$item_location_7_y_from_center <- dataset1_expt1$item_location_7_y - 410 #pixels from the mean
dataset1_expt1$item_location_8_x_from_center <- dataset1_expt1$item_location_8_x - 522 #pixels from the mean
dataset1_expt1$item_location_8_y_from_center <- dataset1_expt1$item_location_8_y - 410 #pixels from the mean

#Color values are as follows:
#Red (1): 255 0 0 , 
#Green (2): 0 255 0
#Blue (3): 0 0 255
#Magenta (5): 255 0 255
#Yellow (4): 255 255 0
#Cyan (6): 0 255 255 
#Orange (9): 255, 128, 0
#White (7): 255, 255, 255
#Black (8): 1, 1, 1

#Arrange the dataframe by id and then block
dataset1_expt1 <- dataset1_expt1 %>% arrange(by = id, block)

#Renumber the IDs from 1:135

dataset1_expt1$id_renumbered <- rep(1:135, each = 540) #because there are 135 participants

#Delete the original id column
dataset1_expt1 <- subset(dataset1_expt1, select = -id)
dataset1_expt1$id <- dataset1_expt1$id_renumbered
dataset1_expt1 <- subset(dataset1_expt1, select = -id_renumbered)

#Check the number of unique IDs
length(unique(dataset1_expt1$id)) #should be 135

#Write the full csv file 

dataset1_expt1_final <- subset(dataset1_expt1, select = c('study', 'experiment', 'id', 'tired', 'attention', 'trial_num', 'block', 'trial_num_within_block',
                                       'set_size', 'change', 'response', 'rt', 'fixation_size', 'stim_size', 'stim_duration',
                                       'retention_interval', 'intertrial_interval', 'break_duration', 'min_distance', 'distance_from_monitor', 'first_test_color',
                                       'second_test_color', 'test_location_x_from_center', 'test_location_y_from_center', 'item_color_1', 'item_color_2', 'item_color_3', 'item_color_4',
                                       'item_color_5', 'item_color_6', 'item_color_7', 'item_color_8', 'item_location_1_x_from_center',
                                       'item_location_2_x_from_center', 'item_location_3_x_from_center', 'item_location_4_x_from_center', 'item_location_5_x_from_center',
                                       'item_location_6_x_from_center', 'item_location_7_x_from_center', 'item_location_8_x_from_center', 'item_location_1_y_from_center',
                                       'item_location_2_y_from_center', 'item_location_3_y_from_center', 'item_location_4_y_from_center', 'item_location_5_y_from_center',
                                       'item_location_6_y_from_center', 'item_location_7_y_from_center', 'item_location_8_y_from_center'))

write_csv(dataset1_expt1_final, 'Dataset1_expt1_formatted_data.csv')


#Choose/format rows and columns for dataset1_expt2

dim(dataset1_expt2) #Should be 293880 rows and 48 columns

#Add or standardize columns
dataset1_expt2$stim_size <- 1.37 #average degrees of visual angle
dataset1_expt2$fixation_size <- 0.3 #degrees of visual angle
dataset1_expt2$stim_duration <- 150 #ms
dataset1_expt2$intertrial_interval <- 1000 #ms
dataset1_expt2$retention_interval <- 1000 #ms
dataset1_expt2$break_duration <- 6000 #ms
dataset1_expt2$distance_from_monitor <- 60 #ms
dataset1_expt2$study <- 1 #Xu et al., 2018
dataset1_expt2$experiment <- 2 #Expt 2
dataset1_expt2$rt <-  dataset1_expt2$rt*1000

#create the final location variables
dataset1_expt2$test_location_x_from_center <- dataset1_expt2$test_location_x - 681 #pixels from the mean
dataset1_expt2$test_location_y_from_center <- dataset1_expt2$test_location_y - 425 #pixels from the mean
dataset1_expt2$item_location_1_x_from_center <- dataset1_expt2$item_location_1_x - 681 #pixels from the mean
dataset1_expt2$item_location_1_y_from_center <- dataset1_expt2$item_location_1_y - 425 #pixels from the mean
dataset1_expt2$item_location_2_x_from_center <- dataset1_expt2$item_location_2_x - 681 #pixels from the mean
dataset1_expt2$item_location_2_y_from_center <- dataset1_expt2$item_location_2_y - 425 #pixels from the mean
dataset1_expt2$item_location_3_x_from_center <- dataset1_expt2$item_location_3_x - 681 #pixels from the mean
dataset1_expt2$item_location_3_y_from_center <- dataset1_expt2$item_location_3_y - 425 #pixels from the mean
dataset1_expt2$item_location_4_x_from_center <- dataset1_expt2$item_location_4_x - 681 #pixels from the mean
dataset1_expt2$item_location_4_y_from_center <- dataset1_expt2$item_location_4_y - 425 #pixels from the mean
dataset1_expt2$item_location_5_x_from_center <- dataset1_expt2$item_location_5_x - 681 #pixels from the mean
dataset1_expt2$item_location_5_y_from_center <- dataset1_expt2$item_location_5_y - 425 #pixels from the mean
dataset1_expt2$item_location_6_x_from_center <- dataset1_expt2$item_location_6_x - 681 #pixels from the mean
dataset1_expt2$item_location_6_y_from_center <- dataset1_expt2$item_location_6_y - 425 #pixels from the mean
dataset1_expt2$item_location_7_x_from_center <- dataset1_expt2$item_location_7_x - 681 #pixels from the mean
dataset1_expt2$item_location_7_y_from_center <- dataset1_expt2$item_location_7_y - 425 #pixels from the mean
dataset1_expt2$item_location_8_x_from_center <- dataset1_expt2$item_location_8_x - 681 #pixels from the mean
dataset1_expt2$item_location_8_y_from_center <- dataset1_expt2$item_location_8_y - 425 #pixels from the mean

#Color values are as follows:
#Red (1): 255 0 0 , 
#Green (2): 0 255 0
#Blue (3): 0 0 255
#Magenta (5): 255 0 255
#Yellow (4): 255 255 0
#Cyan (6): 0 255 255 
#Orange (9): 255, 128, 0
#White (7): 255, 255, 255
#Black (8): 1, 1, 1

#Reorder and standardize the session names and change the 'session' variable to 'day'
dataset1_expt2$day = dataset1_expt2$session
dataset1_expt2 <- subset(dataset1_expt2, select = -session)

#Arrange the dataframe by id and then day
dataset1_expt2 <- dataset1_expt2 %>% arrange(by = id, day)

#Renumber the sessions from 1:31 for each participant

dataset1_expt2$session <- rep(1:31, times = 79, each = 120) #because there are 31 sessions per participant and 120 trials per session

#Check the number of unique IDs
length(unique(dataset1_expt2$id)) #should be 79


#Write the full csv file

dataset1_expt2_final <- subset(dataset1_expt2, select = c('study', 'experiment', 'id', 'day', 'session', 'tired', 'attention', 'trial_num', 'block', 'trial_num_within_block',
                                       'set_size', 'change', 'response', 'rt', 'fixation_size', 'stim_size', 'stim_duration',
                                       'retention_interval', 'intertrial_interval', 'break_duration', 'min_distance', 'distance_from_monitor', 'first_test_color',
                                       'second_test_color', 'test_location_x_from_center', 'test_location_y_from_center', 'item_color_1', 'item_color_2', 'item_color_3', 'item_color_4',
                                       'item_color_5', 'item_color_6', 'item_color_7', 'item_color_8', 'item_location_1_x_from_center',
                                       'item_location_2_x_from_center', 'item_location_3_x_from_center', 'item_location_4_x_from_center', 'item_location_5_x_from_center',
                                       'item_location_6_x_from_center', 'item_location_7_x_from_center', 'item_location_8_x_from_center', 'item_location_1_y_from_center',
                                       'item_location_2_y_from_center', 'item_location_3_y_from_center', 'item_location_4_y_from_center', 'item_location_5_y_from_center',
                                       'item_location_6_y_from_center', 'item_location_7_y_from_center', 'item_location_8_y_from_center'))

write_csv(dataset1_expt2_final, 'Dataset1_expt2_formatted_data.csv')


#Choose/format rows and columns for dataset2

#Add additional variables
dataset2$stim_size <- 0.4 #degrees of visual angle
dataset2$fixation_size <- 1.3 #degrees of visual angle
dataset2$retention_interval <- 900 #ms
dataset2$distance_from_monitor <- 60 #cm
dataset2$study <- 2 #Balaban et al., 2019
dataset2$experiment <- 1 
dataset2$session <- 1 #Although we dont know whether they were a repeat participant or not
dataset2$block <- 1 
dataset2$trial_num_within_block <-  dataset2$trial_num
dataset2$test_location_x_from_center <-  dataset2$test_location_x
dataset2$test_location_y_from_center <-  dataset2$test_location_y

#dataset2$trial_num <- dataset2$trial_num + 1 #(if trials number from 0 rather than 1)
#dataset2$trial_num_within_block <- dataset2$trial_num_within_block + 1 #(if trials number from 0 rather than 1)

dataset2 = subset(dataset2, select = c('study', 'experiment', 'id', 'session', 'trial_num', 'block', 'trial_num_within_block', 'set_size', 'change', 'response', 'rt', 'fixation_size', 'stim_size', 'stim_duration', 'retention_interval', 'distance_from_monitor', 'test_location_x_from_center', 'test_location_y_from_center'))
length(unique(dataset2$id)) #should be 3838 ids

###########################################################################################################
#Create and write the combined dataset
###########################################################################################################

dataset1_expt1_subset <- subset(dataset1_expt1, select = c('study', 'experiment', 'id', 'session', 'trial_num', 'block', 'trial_num_within_block','set_size', 'change', 'response', 'rt', 'fixation_size', 'stim_size', 'stim_duration','retention_interval', 'distance_from_monitor', 'test_location_x_from_center', 'test_location_y_from_center'))
dataset1_expt2_subset <- subset(dataset1_expt2, select = c('study', 'experiment', 'id', 'session', 'trial_num', 'block', 'trial_num_within_block','set_size', 'change', 'response', 'rt', 'fixation_size', 'stim_size', 'stim_duration','retention_interval', 'distance_from_monitor', 'test_location_x_from_center', 'test_location_y_from_center'))
dataset2_subset <- subset(dataset2, select = c('study', 'experiment', 'id', 'session', 'trial_num', 'block', 'trial_num_within_block','set_size', 'change', 'response', 'rt', 'fixation_size', 'stim_size', 'stim_duration','retention_interval', 'distance_from_monitor', 'test_location_x_from_center', 'test_location_y_from_center'))


data_combined <- rbind(dataset1_expt1_subset, dataset1_expt2_subset, dataset2_subset)
data_combined$block[is.na(data_combined$block)] <- 1
#Create the unique id for each dataset in the combined file
data_combined$id <- as.numeric(data_combined$id)
data_combined <- data_combined %>% ungroup() %>%
  mutate(group = cumsum(id != lag(id, default = first(id))) + 1)

#Delete the original id column and replace it with the group column
data_combined <- subset(data_combined, select = -c(id))
data_combined$id <- data_combined$group
data_combined <- subset(data_combined, select = -c(group))

# Move the id col to the right place
data_final <- data_combined %>% relocate(id, .after = experiment)

head(data_final)
dim(data_final) #Should be 827646 rows and 18 columns
length(unique(data_final$id)) #should be 4052 ids

#Write the combined csv file
write_csv(data_final, 'Combined_trial_data.csv')
