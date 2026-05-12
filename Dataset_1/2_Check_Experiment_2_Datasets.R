
## This script checcks that the dataset made in MATLAB and the original csv datasets in the shared folder are the same

#There are 120 extra rows in the shared csv files from the participant 38, session 29, place 3 (382903_ColorK.mat) that cannot be loaded in the shared matlab files.
#Missing data was handled differently, so omit participants 10, 65, 74 and 79 from the csv folder before checking, because these participants were missing sessions/had makeup sessions.

#Create a dataframe from the shared participant csv files

# 1. List all CSV files in the directory
csv_files <- list.files(path = '/Path/to/main_directory/Data_experiment_2/CSV_individuals_E2/', pattern = "*_ColorK.csv", full.names = TRUE)

# 2. Read all files and bind them into one data frame
combined_data <- csv_files %>%
  map_dfr(read_csv)

# 3. Make the ID column

id_col <- data.frame(matrix(NA, nrow = 75, ncol = 1))

for (i in 1:length(csv_files)) {
  id = substr(csv_files[i], 81, 82)
  id_col[i,1] <- id
}

colnames(id_col) <- 'id'
id_col$id <- gsub('_', '', id_col$id)
id_col <- rep(id_col$id[1:75], each = 3720)
id_col <- as.data.frame(id_col)
colnames(id_col) <- 'id'
id_col$id <- as.numeric(id_col$id)
id_col <- id_col %>% filter(id != 38)

#Join the ID column with the rest of the csv data
combined_data <- cbind(id_col, combined_data)


# 4. Write the combined data to a single new CSV file
write_csv(combined_data, '/Path/to/main_directory/Data_experiment_2/CSV_individuals_E2/data_orig.csv')

data_orig <- read.csv('/Path/to/main_directory/Data_experiment_2/CSV_individuals_E2/data_orig.csv')
backup_data_orig <- data_orig

#Read the csv file written from the MATLAB files 
data <- read.csv('/Path/to/main_directory/Experiment_2_trial_data.csv')
backup_data <- data

#Missing data was handled differently between the csv and matlab files, so, before checking, omit the participants with missing data
data$id <- as.numeric(data$id)
data <- data %>% filter(id != 10)
data <- data %>% filter(id != 38)
data <- data %>% filter(id != 65)
data <- data %>% filter(id != 74)
data <- data %>% filter(id != 79)

#Get rid of extra columns in the datasets so that all that remains are what both have in common

data <- data[,c(1:3, 7:11, 13)]
data_orig <- data_orig[,-c(7)]


colnames(data) <- c("id", "session", "place", "trial_num", "block", "trial_num_within_block", "set_size", "change", "rt")
colnames(data_orig) <- c("id", "session", "place", "trial_num", "trial_num_within_block", "block", "set_size", "change", "rt")

#Reorder columns so they are the same

data <- data[, c("id", "session", "place", "trial_num", "trial_num_within_block", "block", "set_size", "change", "rt")]
data_orig <- data_orig[, c("id", "session", "place", "trial_num", "trial_num_within_block", "block", "set_size", "change", "rt")]

#Round off the RT variable

data$rt <- round(data$rt, 2)
data_orig$rt <- round(data_orig$rt, 2)

#Sort both dataframes by ID 
data_orig <- as.data.frame(data_orig)
data <- as.data.frame(data)

#Reorder the ids so that they match
data <- data %>%
  arrange(id, session, block, trial_num_within_block)

data_orig <- data_orig %>%
  mutate(session = replace(session, session == 31, 60)) %>%
  arrange(id, session, block, trial_num_within_block)

#Check to see if both datasets (the dataset compiled from the shared CSV files and the CSV file written to the main directory) are exactly the same. 
all.equal(data, data_orig) #If no other errors, then the dataframes are the same. Move forward from here to explore the data.

#It still necessary to check the participants with missing data separately (10, 38, 65, 74, 79).
#Prior to analyzing the data, the missing session 29 for 38 was added to the dataset from the corresponding csv file. 

