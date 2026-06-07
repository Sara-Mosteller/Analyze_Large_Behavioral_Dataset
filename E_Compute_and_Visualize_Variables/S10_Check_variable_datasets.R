###########################################################################################################
#Check variable datasets
###########################################################################################################

#This script checks the dimensions of dataframes made in Python and R are the same.
#A correlation matrix is output to confirm that the variables computed in R and Python are strongly related.

###########################################################################################################
#Import the needed packages
###########################################################################################################

#install.packages(dplyr)
library(dplyr)
#install.packages("lineup")
library(lineup)
#install.packages("corrplot")

###########################################################################################################
#Read in the dataset and, if needed, get rid of extra columns. Set the data types.
###########################################################################################################

trial_data_R <- read.csv('Path/to/filtered_trial_data.csv')
variable_data_R <- read.csv('/Path/to/analysis_variables_without_outliers_grouped_by_id_and_set_size.csv')
trial_data_Python <- read.csv('/Path/to/filtered_trial_data.csv')
variable_data_Python <- read.csv('/Path/to/analysis_variables_grouped_by_id_and_set_size.csv')


#Get rid of extra columns
dim(trial_data_R)
#trial_data_R <- trial_data_R[,-c(1)]
dim(trial_data_Python)
#trial_data_Python <- trial_data_Python[,-c(1)]

dim(variable_data_R)
#variable_data_R <- variable_data_R[,-c(1)]
dim(variable_data_Python)
#variable_data_Python <- variable_data_Python[,-c(1)]

#Set the data types for the later matrix
variable_data_R <- variable_data_R %>% mutate(across(all_of(c("id")), as.factor)) #leave out set size since variable dataframes might not be grouped by set size
variable_data_R <- variable_data_R %>% mutate(across(all_of(c("trial_count", "rt_mean", "rt_sd", "rt_min", "rt_q1", "rt_median", "rt_q3", "rt_max", "hits", "false_alarms", 
                                                              "correct_rejections", "misses", "accuracy", "hit_rate", "false_alarm_rate", "correct_rejection_rate", "miss_rate",
                                                              "dprime", "aprime", "response_bias", "response_bias_probability", "k")), as.numeric))

#Set the data types for the covariance matrix
variable_data_Python <- variable_data_Python %>% mutate(across(all_of(c("id")), as.factor))
variable_data_Python <- variable_data_Python %>% mutate(across(all_of(c("trial_count", "rt_mean", "rt_sd", "rt_min", "rt_q1", "rt_median", "rt_q3", "rt_max", "hits", "false_alarms", 
                                                              "correct_rejections", "misses", "accuracy", "hit_rate", "false_alarm_rate", "correct_rejection_rate", "miss_rate",
                                                              "dprime", "aprime", "response_bias", "response_bias_probability", "k")), as.numeric))




###########################################################################################################
#Check the datasets
###########################################################################################################

#Check for missing rt data (which shows whether outliers were omitted or not)

na_rt_values_R <- trial_data_R %>% filter(is.na(rt)) 
na_rt_values_Python <- trial_data_Python %>% filter(is.na(rt)) #These aren't exactly the same when outliers are removed but they are close

#Check that the ID columns are equal: 
all.equal(trial_data_R$id, trial_data_Python$id) 

#Compute a covariance matrix of the remaining variables

R_analysis_variables <- variable_data_R %>% dplyr::select(where(is.numeric))
Python_analysis_variables <- variable_data_Python %>% dplyr::select(where(is.numeric))
cov_matrix <- cov(R_analysis_variables, Python_analysis_variables, use = "pairwise.complete.obs")

#Plot a correlation matrix 

# Calculate correlations between columns of R_analysis_variables and Python_analysis_variables
#The result should be a dark blue line of squares down the diagonal (because r is nearly = 1)

cor_matrix <- corbetw2mat(as.matrix(R_analysis_variables), as.matrix(Python_analysis_variables), what = "all")

# Plot the resulting correlation matrix
# Note: lineup does not have a direct plot function for the matrix itself,
# so you can use corrplot or base R to visualize the output matrix
if(requireNamespace("corrplot", quietly = TRUE)) {
  library(corrplot)
  corrplot(cor_matrix, method = "color", type = "full", 
           title = "Correlations between columns of R_analysis_variables and Python_analysis_variables")
} else {
  # Fallback to base R heatmap if corrplot is not available
  image(cor_matrix, main = "Correlations between columns of R_analysis_variables and Python_analysis_variables")
}

#Option to check the actual values at the beginning and end of the dataframe. 

head(variable_data_Python) #examine the first few rows
head(variable_data_R)
tail(variable_data_Python) #examine the last few rows
tail(variable_data_R)
