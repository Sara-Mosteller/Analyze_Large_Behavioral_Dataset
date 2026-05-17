###########################################################################################################
#This script checks that all of the datasets made by S7 are the same between the R and Python versions
###########################################################################################################

#Read the datasets made in R
dataset1_expt1_R <- read.csv('Path/to/Dataset1_expt1_formatted_data.csv')
dataset1_expt2_R <- read.csv('Path/to/Dataset1_expt2_formatted_data.csv')
data_combined_and_trimmed_R <- read.csv('Path/to/Combined_trial_data_with_120_4or8_trials_per_individual_dataset.csv')
data_combined_full_R <- read.csv('/Path/to/Combined_trial_data_with_all_trials_in_all_datasets.csv')

#Read the datasets made in Python
dataset1_expt1_Python <- read.csv('Path/to/Dataset1_expt1_formatted_data.csv')
dataset1_expt2_Python <- read.csv('Path/to/Dataset1_expt2_formatted_data.csv')
data_combined_and_trimmed_Python <- read.csv('Path/to/Combined_trial_data_with_120_4or8_trials_per_individual_dataset.csv')
data_combined_full_Python <- read.csv('/Path/to/Combined_trial_data_with_all_trials_in_all_datasets.csv')

#Delete unnecessary columns

dim(dataset1_expt1_R)
dim(dataset1_expt1_Python) #If not the same, delete the extra column(s)
#dataset1_expt1_Python <- dataset1_expt1_Python[,-1]

dim(dataset1_expt2_R)
dim(dataset1_expt2_Python) #If not the same, delete the extra column(s)
#dataset1_expt2_Python <- dataset1_expt2_Python[,-1]

dim(data_combined_and_trimmed_R)
dim(data_combined_and_trimmed_Python) #If not the same, delete the extra column(s)
#data_combined_and_trimmed_Python <- data_combined_and_trimmed_Python[,-1]

dim(data_combined_full_R)
dim(data_combined_full_Python) #If not the same, delete the extra column(s)
data_combined_full_Python <- data_combined_full_Python[,-1]


#Check
all.equal(dataset1_expt1_R, dataset1_expt1_Python) #If everything is the same, then move on to explore the data
all.equal(dataset1_expt2_R, dataset1_expt2_Python) #If everything is the same, then move on to explore the data
all.equal(data_combined_and_trimmed_R, data_combined_and_trimmed_Python) #If everything is the same, then move on to explore the data
all.equal(data_combined_full_R, data_combined_full_Python) #If everything is the same, then move on to explore the data
