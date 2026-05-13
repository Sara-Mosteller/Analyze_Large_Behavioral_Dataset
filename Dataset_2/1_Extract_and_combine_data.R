#This script compiles and formats the trial-level data from Balaban et al. 2019
#The data and documentation can be found and downloaded at: https://osf.io/mzs9e/


###########################################################################################################
#Read all of the files from the main folder.
###########################################################################################################

#Navigate to the main K folder 

#List all of the sub-directories within the directory

files <- list.files(full.names = T , recursive = T, pattern='.*._ChangeDetection_BIGT.txt')

datalist = lapply(files, function(x)read.table(x, header=F)) 

datafr = do.call("rbind", datalist) 

colnames(datafr) <- c("PartID", "set_size", "stim_duration", "change_orig", "response_orig", "rt", "test_location_x", "test_location_y")

#This creates a data frame of all of the participant IDs and sessions in the study
print(files)
lapply(files, write, "R_File_List.txt", append=TRUE, ncolumns=1)


###########################################################################################################
#Use the index from the list as the new ID, so that each text file is a new 'participant'
#Take only participants with fewer than 130 trials in the dataset.
###########################################################################################################


names(datalist) <- seq_along(datalist)
data = do.call("rbind", datalist[])
data = do.call(rbind.data.frame, datalist[]) #This prints out the data frame with indices

#Turn the row headers into a column
data <- rownames_to_column(data, var = "row_names")

colnames(data) <- c("id", "PartID", "set_size", "stim_duration", "change_orig", "response_orig", "rt", "test_location_x", "test_location_y")

#Remove decimals
data$id <- as.numeric(data$id)
data$id <- trunc(data$id)
datacount <- data %>% group_by(id) %>% count()

#Convert ID to factor for analysis
data$id <- as.factor(data$id)

#Keep the first 120 trials from each participant ID as one option

trimmed_dataset <- data %>% group_by(id) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum <= 120)

#check to make sure that every dataset has no more than 120 trials

datafr2 <- trimmed_dataset %>% group_by(id) %>% count()
datafr3 <- datafr2 %>% filter(n > 120)


#Make the first (raw) trial dataset

data <- data %>% group_by(id) %>%
  mutate(rownum = row_number()) 

datafr2 <- data %>% group_by(id) %>% count()
datafr3 <- datafr2 %>% filter(n > 130) #0 obs

#It looks like they already omitted all sessions with > 130 trials, therefore, the final dataset before filtering 
#down to 120 trials is the one for reproducing the original study and conducting the analysis


###########################################################################################################
#The result should be a dataframe with 462186 observation indices (original) or 461880 observation indices (trimmed) 
#and 3849 unique dataset IDs. 
###########################################################################################################

nrow(trimmed_dataset) #Check the number of rows
nrow(data)

#Write the dataset as a file for analysis

write.csv(trimmed_dataset, "trimmed_dataset.csv")

write.csv(data, "raw_trial_data.csv")


###########################################################################################################
#Format the data
###########################################################################################################

#Format the main dataframe into a wide form with all 101 and 102 trials coded in the same variable as 1 and 2 trials

data$response <- NA
data$change <- NA

#Get rid of 37 trials where the recorded response was "103" by un-commenting the line below

#data <- data %>% filter(Response != 103)

#Alternatively, delete these datasets from the dataframe:

odd_response_IDs <- data %>% filter(response_orig == 103) %>% dplyr::select(id)

data <- data %>% filter(! id %in% odd_response_IDs$id) #There are now 460866 rows and 3838 IDs


data <- data %>%
  mutate(response = replace(response, response_orig == 1, 0)) %>%
  mutate(response = replace(response, response_orig == 2, 1)) %>%
  mutate(response = replace(response, response_orig == 101, 0)) %>%
  mutate(response = replace(response, response_orig == 102, 1)) %>%
  mutate(change = replace(change, change_orig == 0, 0)) %>%
  mutate(change = replace(change, change_orig == 5, 1))

data$response <- as.factor(data$response)
data$change <- as.factor(data$change)

names(data)[names(data) == "rownum"] <- "trial_num"

data_final <- data[, c("id", "trial_num", "set_size", "stim_duration", "change", "response", "rt", "stim_duration", "test_location_x", "test_location_y")]


write.csv(data_final, "Trial_Data.csv")
