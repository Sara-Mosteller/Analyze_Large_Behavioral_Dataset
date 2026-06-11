###########################################################################################################
#This script creates variables from aggregating the trial-level data based on the specified grouping factors
###########################################################################################################


#From the response variable: hit, false alarm, correct rejection, and miss rates are computed as well as
#sensitivity to a true change (d' and A'), response bias (to choose 'different'), and capacity (K).
#Mean, standard deviation, and quantile values are computed for the reaction times.
#If no grouping variable is specified, the script will group the original dataset by id. 
#Option included to additionally group by the array set size. 

#Parameters for the functions must be set below before running the script.

###########################################################################################################
#Import the packages.
###########################################################################################################

#install.packages(dplyr)
library(dplyr)
#install.packages(reshape2)
library(reshape2)
#install.packages(tidyverse)
library(tidyverse)

###########################################################################################################
#Read in the dataset and, if needed, get rid of extra columns. Check for missing data.
###########################################################################################################

data <- read.csv('Path/to/Combined_trial_data.csv')

head(data)

#Run if there is an extra column (in this case, at the beginning)
#data <- data[,-1]
#head(data)

sum(is.na(data$change)) #The change variable doesn't have any missing values
sum(is.na(data$response)) #The response variable doesn't have any missing values
sum(is.na(data$rt)) #The reaction time also doesn't have any missing values

#Check for manually added values for missing data (e.g., reaction time = 999999)

extreme_values <- data %>% filter(rt > 10000) #There are very high values but they seem to all be legitimate data. 

###########################################################################################################
#Set the parameters to be used by the functions.
###########################################################################################################

group_by_set_size <- TRUE #If TRUE, the dataset will be grouped by set size, else enter FALSE to only group by id
filter_set_sizes <- FALSE #If TRUE, input which set sizes to omit below
set_sizes_to_omit <- c(6)
filter_rows <- FALSE #If TRUE, input the max rows per participant within each set size. Row numbers larger than this will be omitted.
max_row_number <- 60
omit_rt_outliers <- FALSE #Include this if you want the variables to be computed after omitting extreme reaction time values. 

###########################################################################################################
#Omit outlying RT values (if specified)
###########################################################################################################

remove_outliers_na <- function(x) {
  stats <- boxplot.stats(x)$out
  ifelse(x %in% stats, NA, x)
}

if (omit_rt_outliers == TRUE) {
  data <- data %>%
    group_by(id) %>%
    mutate(rt = remove_outliers_na(rt)) %>%
    ungroup()
} else { print("All reaction time values were included.") }

###########################################################################################################
#Trim the datasets to reflect the desired set sizes and number of trials per participant and set size.
###########################################################################################################

trim_dataset <- function(df, filter_set_sizes, filter_rows) {
  
  #' Filter dataset by the specified max number of rows within each id and set size and the desired set sizes
  #'
  #' @param df A data frame (data)
  #' @param filter_set_sizes If TRUE, the set sizes specified in set_sizes_to_omit will be omitted from the dataset. 
  #' If FALSE, all set sizes will be included.  
  #' @param filter_rows If TRUE, once the dataframe has been grouped by id and set size, 
  #' only the max number of rows will be retained. If FALSE, all rows will be retained.   
  #' @return A filtered dataframe. 
  
  if (filter_rows == TRUE) {
    df <- df %>% slice_head(n = max_row_number) 
    print(paste0('Row numbers > ', max_row_number, ' within each set size were omitted.'))
  } else {
    print(paste0("All rows were included."))
  }

  if (filter_set_sizes == TRUE) {
    for (size in set_sizes_to_omit) {
      df <- df %>% filter(set_size != size)
      print(paste0('Set size ', size, ' was omitted.')) }
  } else {
    print(paste0("All set sizes were included."))
  }
    
  #filtered_data <- df
  #assign("filtered_data", filtered_data, envir = .GlobalEnv) 

  return (df)
}


#Call the function on the data

filtered_data <- data %>%
  group_by(id,set_size) %>%
  trim_dataset(filter_set_sizes, filter_rows) %>%
  ungroup()

#Print out the dimensions of the new, filtered dataset. (If filter_rows and filter_set_sizes are both set to FALSE,
#then the dataframe will have its original dimensions.)
dim(filtered_data)

###########################################################################################################
#Compute the mean, standard deviation, and quantile values of a numeric variable (rt).
###########################################################################################################

numeric_variable_stats <- function(df, value_col) {
  
  #' Calculate quantitative variables of a numeric variable (such as reaction time)
  #'
  #' This function computes the arithmetic mean, standard deviation, and quantile values (0%, 25%, 50%, 75%, 100%) 
  #' of a numeric vector, handling missing values based on the na.rm parameter.
  #'
  #' @param df A data frame.
  #' @param value_col The column in the dataframe over which the computations will be performed (rt)
  #' @return A dataframe summarizing these variables for each level of grouping specified (id, id+set_size)

  if (group_by_set_size == TRUE) {
      df <- df %>%
      group_by(id,set_size) %>%
      summarise(
        rt_mean = mean({{value_col}}, na.rm = TRUE),
        rt_sd = sd({{value_col}}, na.rm = TRUE),
        rt_min = quantile({{value_col}}, 0.0, na.rm = TRUE),
        rt_q1 = quantile({{value_col}}, 0.25, na.rm = TRUE),
        rt_median = quantile({{value_col}}, 0.50, na.rm = TRUE),
        rt_q3 = quantile({{value_col}}, 0.75, na.rm = TRUE),
        rt_max = quantile({{value_col}}, 1.0, na.rm = TRUE),
        .groups = 'drop'
      )
      print("The dataframe was grouped by id and set size.")
  }
  else {
        df <- df %>%
        group_by(id) %>%
        summarise(
          rt_mean = mean({{value_col}}, na.rm = TRUE),
          rt_sd = sd({{value_col}}, na.rm = TRUE),
          rt_min = quantile({{value_col}}, 0.0, na.rm = TRUE),
          rt_q1 = quantile({{value_col}}, 0.25, na.rm = TRUE),
          rt_median = quantile({{value_col}}, 0.50, na.rm = TRUE),
          rt_q3 = quantile({{value_col}}, 0.75, na.rm = TRUE),
          rt_max = quantile({{value_col}}, 1.0, na.rm = TRUE),
          .groups = 'drop'
        )
        print("The dataframe was grouped by id.")
    }
  
  return (df)
}

#Call the function on the filtered data
numeric_output <- filtered_data %>% numeric_variable_stats(rt)

###########################################################################################################
#Compute the trial counts by condition (change * response).
###########################################################################################################

categorical_variable_counts <- function(df, value_col) {
  
  #' Calculate counts of different classes of a categorical variable (such as change + response)
  #'
  #' This function computes the number of hits, false alarms, correct rejections, and misses.
  #' Hits: change = 1, response = 1
  #' False alarms: change = 0, response = 1
  #' Correct rejections: change = 0, response = 0
  #' Misses: change = 1, response = 0
  #' @param df A data frame (expecting filtered_data)
  #' @param value_col It does not matter what column is specified as long as it has no missing values, since the function simply counts rows 
  #' @return A wide dataframe with counts of these outcomes for each level of grouping specified (id, id+set_size)
  
  if (group_by_set_size == TRUE) {
    df$id <- as.factor(df$id)
    df$set_size <- factor(df$set_size, levels = c(4,6,8))
    df$change <- factor(df$change, levels = c(0,1))
    df$response <- factor(df$response, levels = c(0,1))
    df <- df %>% group_by(id, set_size, change, response, .drop=FALSE) %>% count() %>% ungroup()
    df <- dcast(df, id + set_size ~ change + response, value.var = 'n')
    colnames(df) <- c('id', 'set_size', 'correct_rejections', 'false_alarms', 'misses', 'hits')
    df$trial_count <- df$correct_rejections + df$false_alarms + df$misses + df$hits
    df <- df %>% filter(trial_count != 0) #get rid of extra set size 6 rows in the dataset 2
    print("The dataframe was grouped by id and set size.")
  }
  else {
    df$id <- as.factor(df$id)
    df$change <- factor(df$change, levels = c(0,1))
    df$response <- factor(df$response, levels = c(0,1))
    df <- df %>% group_by(id, change, response, .drop=FALSE) %>% count() %>% ungroup()
    df <- dcast(df, id ~ change + response, value.var = 'n')
    colnames(df) <- c('id', 'correct_rejections', 'false_alarms', 'misses', 'hits')
    df$trial_count <- df$correct_rejections + df$false_alarms + df$misses + df$hits
    print("The dataframe was grouped by id.")
  }
  
  return (df)
}

#Call the function on the filtered data
counts <- filtered_data %>% categorical_variable_counts(trial_num)

###########################################################################################################
#Create the final variables for analysis.
###########################################################################################################

create_analysis_variables <- function(df) {
  
  #' Compute final analysis variables based on counts of hits, false alarms, correct rejections, and misses
  #' Final variables include hit rate, false alarm rate, correct rejection rate, and miss rate, as well as 
  #' accuracy, d', A', response bias, and K.
  #' accuracy is the # correct responses / # responses
  #' d' is the sensitivity to a true change, computed as z(hit rate) - z(false alarm rate)
  #' A' is a non-parametric measure of sensitivity. 
  #' K is a measure of capacity that approximates how many items are simultaneously 
  #' held in working memory. K can only be computed if the dataframe was grouped by set size. 
  #' Otherwise, an empty column is returned. 
  #' 
  #' @param df A dataframe (expecting counts)
  #' @return A dataframe with all of the above analysis variables for each level of grouping specified (id, id+set_size)

  #Compute rate variables with a correction for extreme values
  df$accuracy <- (df$hits + df$correct_rejections + .5)/(df$hits + df$correct_rejections + df$false_alarms + df$misses + 1)
  df$hit_rate <- (df$hits + .5) /(df$hits + df$misses + 1)
  df$false_alarm_rate <- (df$false_alarms + .5) / (df$correct_rejections + df$false_alarms + 1)
  df$correct_rejection_rate <- (df$correct_rejections + .5) / (df$correct_rejections + df$false_alarms + 1)
  df$miss_rate <- (df$misses + .5) / (df$hits + df$misses + 1)

  #Compute analysis variables

  #d' (sensitivity to a true change
  df$dprime <- qnorm(df$hit_rate) - qnorm(df$false_alarm_rate)
  
  #A' as a non-parametric alternative to d'
  df$aprime = ifelse(df$hit_rate >= df$false_alarm_rate,
                        (.5 + (((df$hit_rate - df$false_alarm_rate) * (1 + df$hit_rate - df$false_alarm_rate)) / (4 * df$hit_rate * (1 -  df$false_alarm_rate)))),
                        (.5 - (((df$false_alarm_rate - df$hit_rate) * (1 + df$false_alarm_rate - df$hit_rate)) / (4 * df$false_alarm_rate * (1 -  df$hit_rate)))))

  #Response bias to choose 'different' with a correction for extreme values
  df$response_bias <- (df$hits + df$false_alarms + .5)/(df$hits + df$correct_rejections + df$false_alarms + df$misses + 1)
  df$response_bias_probability <- (qnorm(df$hit_rate) + qnorm(df$false_alarm_rate))/2

  #Paschler's K, a measure of capacity that should remain stable across set sizes
  #This function has been modified such that values < 1 are diminishing positive fractions. 
  if (group_by_set_size == TRUE) {
    df$set_size <- as.numeric(levels(df$set_size))[df$set_size]
    df$k_temp <- df$set_size * (df$hit_rate - df$false_alarm_rate) / (1 - df$false_alarm_rate)
    df$k <- ifelse(df$k_temp < 1, 
                   1/(1+exp(-df$k_temp)),
                   df$k_temp)
    df <- df %>% select(-k_temp)
  }
  else {
    df$k = ''
    print("Enter 'group_by_set_size <- TRUE' to compute K.")
  }
  
  return (df)
}

#Run the function on the categorical dataset to create the new variables
analysis_variables <- create_analysis_variables(counts)

###########################################################################################################
#Combine and save the full dataset with variables.
###########################################################################################################
analysis_variables$id <- as.numeric(analysis_variables$id)
analysis_variables$study <- ifelse((analysis_variables$id < 215), 1, 2)
analysis_variables$experiment <- ifelse((analysis_variables$id <= 135 | analysis_variables$id >= 215), 1, 2)

if (group_by_set_size == TRUE) {
  final_analysis_variables <- cbind(numeric_output, analysis_variables[,-c(1:2)]) #subtracts the id and set size column from analysis_variables
  colnames <- c("study", "experiment", "id", "set_size", "trial_count", "rt_mean", "rt_sd", "rt_min", "rt_q1", "rt_median", "rt_q3", "rt_max", "hits", "false_alarms", 
                "correct_rejections", "misses", "accuracy", "hit_rate", "false_alarm_rate", "correct_rejection_rate", "miss_rate",
                "dprime", "aprime", "response_bias", "response_bias_probability", "k")
  
} else {
  final_analysis_variables <- cbind(numeric_output, analysis_variables[,-c(1)]) #subtracts the id column from analysis_variables
  colnames <- c("study", "experiment", "id", "trial_count", "rt_mean", "rt_sd", "rt_min", "rt_q1", "rt_median", "rt_q3", "rt_max", "hits", "false_alarms", 
                "correct_rejections", "misses", "accuracy", "hit_rate", "false_alarm_rate", "correct_rejection_rate", "miss_rate",
                "dprime", "aprime", "response_bias", "response_bias_probability", "k")
  
}

final_analysis_variables <- final_analysis_variables[, colnames]

#Write the dataset to a csv file with the grouping variables in the title.

print(paste0('data was grouped by set size ', group_by_set_size))
print(paste0('filtering by set size was set to ', filter_set_sizes, ' and filtering rows was set to ', filter_rows))
print(paste0('set size(s) ', set_sizes_to_omit, ' present in set_sizes_to_omit and row number ', max_row_number, ' was listed as the max_row_number within each set size.'))
print(paste0('The dataset with trials has dimensions: ', dim(filtered_data)))
print(paste0('The final dataset with variables has dimensions: ', dim(final_analysis_variables)))

if (group_by_set_size == TRUE && omit_rt_outliers == TRUE) {
filename = 'analysis_variables_without_outliers_grouped_by_id_and_set_size.csv'
} else if (group_by_set_size == TRUE && omit_rt_outliers == FALSE) { 
  filename = 'analysis_variables_grouped_by_id_and_set_size.csv'
} else if (group_by_set_size == FALSE && omit_rt_outliers == TRUE) { 
  filename = 'analysis_variables_without_outliers_grouped_by_id.csv'
} else if (group_by_set_size == FALSE && omit_rt_outliers == FALSE) { 
  filename = 'analysis_variables_grouped_by_id.csv' }

#Write the csv file with the analysis variables
write.csv(final_analysis_variables, filename)

#Option to write the trimmed trial-level dataset to a csv file:
write_csv(filtered_data, "filtered_trial_data.csv")
