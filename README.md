# Analyse a large behavioural dataset

> This repository provides a complete pipeline for compiling, processing, and visualising previously shared datasets of a visual working memory task. Once the raw data have been downloaded, the scripts walk step-by-step in order of numbering through this full process, from compiling and checking the raw data to visualising the results. 

Scripts are available in both R and Python versions, with equivalent scripts having the same name. 

#Data sources

Raw data is not included in this repository.

#Dataset 1 

>Dataset 1 contains two experiments from the following paper, referred to as Experiment 1
and Experiment 2:

**Xu, Z., Adam, K. C. S., Fang, X., & Vogel, E. K. (2018). The reliability and stability of
visual working memory capacity. Behavior Research Methods, 50(2), 576-588

The data and corresponding documentation can be accessed here: **https://osf.io/g7txf/

The scripts expect the following structure:

```
main_directory/
  Analysis_Code_Experiment_1 /    
  Analysis_Code_Experiment_2/	
  cellflat.m
  Data_Experiment_1/
  	CSV_individuals/ #Shared participant CSV files from Experiment 1
	IndividualFiles/ #Raw MATLAB participant files from Experiment 1
  Data_Experiment_2/ 
  	CSV_individuals_E2/ #Shared participant CSV files from Experiment 2
	Matlab_individuals_E2/ # Raw MATLAB participant and session files from Experiment 2
  Manuscript/
  Task_Code/
  
```


#Dataset 2

>Dataset 2 contains one experiment and 3849 datasets of 120-130 trials in a single session
from the following paper:

**Balaban, H., Fukuda, K., & Luria, R. (2019). What can half a million change detection trials
tell us about visual working memory? Cognition, 191, 103984

The data and corresponding documentation can be accessed here: **https://osf.io/mzs9e/


The scripts expect the following structure:

```
main_directory/
  K/ #Contains embedded subfolders with the raw text files
  
```

# Prerequisites

#MATLAB

The following function must be saved into the working directory: cellflat.m 

Retrieved from:
https://www.mathworks.com/matlabcentral/fileexchange/50502-flatten-nested-cell-arrays


# Python 3.9+

```bash
pip install -r requirements.txt
```

Packages:`fnmatch`, `itertools`,  `matplotlib`, `numpy`, `os`, `pandas`, `scipy`,  `seaborn`

# R

Required packages: `corrplot`,  `dplyr`, `ggplot2`, `lineup`, `reshape2`, `tidyverse` 

```r
install.packages(c("corrplot",  "dplyr", "ggplot2", "lineup", "reshape2", "tidyverse"))
```


# Repository structure in running order

| Folder | File | Description |
|--------|------|-------------|
| A_Documentation_and_Tools | `Instructions_for_Use.pdf` | Provides full documentation and notes. |
| 			    | `Python_requirements.txt` | Script to install the needed packages for Python scripts |
| 			    | `Check_functions_needed_for_R_script.R` | Lists all functions and packages used in a given R script. |
| B_Compile_Dataset_1 | `S1_Compile_Data_from_Experiment_1_MATLAB_files.m` | Compile a full dataset of features from the raw participant MATLAB files from the first experiment |
| 		      | `S2_Check_experiment_1_trial_datasets.R` | Check for equivalence between the data compiled from the MATLAB files and a compilation from the shared participant csv files. |
| 		      | `S3_Compile_Data_from_Experiment_2_MATLAB_files.m` | Compile a full dataset of features from the raw participant and session MATLAB files from the second experiment |
|	              | `S4_Check_experiment_2_trial_datasets.R` | Check for equivalence between the dataset compiled in Python and in R. |
| C_Compile_Dataset_2 | `S5_Extract_and_combine_data.ipynb` , `S5_Extract_and_combine_data.R` | Compile the raw text files shared online into a single dataset. |
| 		      | `S6_Check_trial_datasets.R` | Check for equivalence between the dataset compiled in Python and in R.  |
| D_Combine_Datasets | `S7_Combine_trial_datasets.ipynb` , `S7_Combine_trial_datasets.R` | Combine dataset 1 (both experiments) with dataset 2 into a complete dataset with overlapping features. Format all datasets for analysis. |
|		     | `S8_Check_combined_datasets.R` | Check for equivalence between the datasets formatted/combined in Python and those in R. |
| E_Compute_and_Visualize_Variables | `S9_Compute_variables.ipynb` , `S9_Compute_variables.R` | Aggregate the data the ID/set size level and compute variables for analysis. |
| 		     		    | `S10_Check_variables.R` | Check for equivalence between the variables computed in Python and in R. |
| 		     		    | `S11_Visualize_variables.ipynb` , `S11_Visualize_variables.R` | Generate plots of the variables. |


# Citation

*TBD — citation information will be added upon publication.*
