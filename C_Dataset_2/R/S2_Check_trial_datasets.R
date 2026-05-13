#Check that the dataset made in R and the dataset made in Python (using the R file list) are the same

library(dplyr)
library(stringr)

########################################################################################################################

#First check the final, trial-level datasets

dataR = read.csv('/Path/to/K/Trial_Data.csv')
backup_dataR <- dataR

dataPython <- read.csv('/Path/to/Trial_Data.csv')
backup_dataPython <- dataPython

#Get rid of extra columns in the datasets

dataR <- dataR[,-c(1)]
dataPython <- dataPython[,-c(1)]


all.equal(dataR, dataPython) #If no other errors, then the dataframes are the same. Move forward from here to explore the data. 
