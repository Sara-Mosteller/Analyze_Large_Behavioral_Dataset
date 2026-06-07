install.packages("NCmisc")
library(NCmisc)

#Check what functions are present in an R script and the packages needed to run the script. 

funcs <- list.functions.in.file(filename = ".R")
print(funcs)