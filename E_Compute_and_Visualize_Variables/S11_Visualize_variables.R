###########################################################################################################
#This script summarizes and visualizes the variables
###########################################################################################################

#Start with a dataframe with variables (e.g, rt_mean, accuracy, d', A', response bias, K)
#already computed for each participant. 

#Parameters must be set below before running the script.
#All plots will be saved as pdf files to the working directory

#Summary of grouping variables: 

#Study = 1: Dataset 1
  #Experiment = 1: Experiment 1
    #Set size = 4: 4 items in the memory array
    #Set size = 6: 6 items in the memory array
    #Set size = 8: 8 items in the memory array
  #Experiment = 2: Experiment 2
    #Set size = 4: 4 items in the memory array
    #Set size = 6: 6 items in the memory array
    #Set size = 8: 8 items in the memory array
#Study = 2: Dataset 2
  #Experiment = 1
    #Set size = 4: 4 items in the memory array
    #Set size = 8: 8 items in the memory array

###########################################################################################################
#Import the packages.
###########################################################################################################

#install.packages(dplyr)
library(dplyr)
#install.packages(ggplot2)
library(ggplot2)
#install.packages(reshape2)
library(reshape2)
#install.packages(tidyverse)
library(tidyverse)

###########################################################################################################
#Read in the dataset and, if needed, get rid of extra columns.
###########################################################################################################

data <- read_csv('Path/to/analysis_variables_grouped_by_id_and_set_size.csv')

head(data)

#Run if there is an extra column (in this case, at the beginning)
#data <- data[,-1]
#head(data)

###########################################################################################################
#Set the parameters to be used by the functions.
###########################################################################################################

grouping_variables <- c('study', 'experiment', 'set_size') #Separate plots will be generated for each interacting group level
variables_to_explore <- c('rt_mean', 'rt_sd', 'rt_min', 'rt_q1', 'rt_median', 'rt_q3', 'rt_max',
                        'accuracy', 'hit_rate', 'false_alarm_rate', 'correct_rejection_rate', 
                        'dprime', 'aprime', 'response_bias', 'response_bias_probability', 'k', 'k_modified')
omit_outliers <- FALSE #If TRUE, input the threshold below
outlier_threshold <- 1.5 #specify in distance from the IQR for outliers to be replaced with NA

#Set the working directory, where the summary table and all plots will be saved. 
#setwd("/path/to/outputs_folder")

###########################################################################################################
#Replace outlying values in the variables_to_explore with NA (if/as specified)
###########################################################################################################

remove_iqr_outliers <- function(data, value_cols = NULL, group_cols = NULL,
                                         outlier_threshold = 1.5) {

  #' Remove outliers using the threshold*IQR rule.

  #' Parameters:
  #' - data : DataFrame
  #'      Input dataframe or grouped dataframe.
  #'  - value_cols : Columns to evaluate. If NULL, all numeric columns are used.
  #'  - group_cols : Columns to group the data frame. If NULL, outliers will be replaced with NA based on the whole column.

  #'  Returns:
  #'  - DataFrame with outlier values within each level of the specified group_cols replaced with NA.

  # If omit_outliers = FALSE, return data unchanged
  if (!omit_outliers) {
    return(data)
  }
  
  # Function to replace outliers with NA in the data frame (or group)
  replace_outliers <- function(df) {
    cols <- value_cols
    if (is.null(cols)) {
      cols <- df %>% select(where(is.numeric)) %>% colnames()
    }
    
    for (col in cols) {
      if (!col %in% names(df) || !is.numeric(df[[col]])) next
      
      q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
      q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      
      lower <- q1 - outlier_threshold * iqr
      upper <- q3 + outlier_threshold * iqr
      
      # Replace values outside bounds with NA, keeping NAs as is
      df[[col]] <- ifelse(df[[col]] >= lower & df[[col]] <= upper | is.na(df[[col]]), 
                          df[[col]], NA_real_)
    }
    return(df)
  }
  
  if (!is.null(group_cols)) {
    # Apply replacement within each group
    data_clean <- data %>%
      group_by(across(all_of(group_cols))) %>%
      group_modify(~ replace_outliers(.x)) %>%
      ungroup()
  } else {
    # Apply replacement to the whole dataset
    data_clean <- replace_outliers(data)
  }
  
  return(data_clean)
}   


#Call the function the data
if (omit_outliers == TRUE) {
  data <- remove_iqr_outliers(data, group_cols = grouping_variables, value_cols = variables_to_explore, outlier_threshold = outlier_threshold)
} else { print("All reaction time values were included.") }

#Check the dimensions
dim(data)

#Check to see how many values in each column have been replaced with NA
colSums(is.na(data[variables_to_explore]))

###########################################################################################################
#Generate a summary table
###########################################################################################################

summary_table <- function(df, value_cols = NULL, group_cols = NULL) {
  
  #' Parameters:
  #'  - data : DataFrame
  #' - value_cols : list-like, optional
  #'      Columns to evaluate. If NULL, all numeric columns are used.
  #'  - group_cols : list-like, optional
  #'      Columns to group the data frame. If NULL, summaries will be based on the whole column.

  #'  Returns:
  #'  - pd.DataFrame
  #'      DataFrame with outliers replaced with NA within each level of the specified grouping variables.

  # If value_cols is NULL, select only numeric columns in the dataframe
  if (is.null(value_cols)) {
    message("All numeric columns were used.")
    numeric_cols <- df %>% select(where(is.numeric)) %>% colnames()
    if (length(numeric_cols) == 0) {
      stop("No numeric columns found in the dataframe.")
    }
    value_cols <- numeric_cols
  }
  
  # Select only the columns needed for summary
  cols_to_use <- c(group_cols, value_cols)
  # Handle case where group_cols is NULL
  if (is.null(group_cols)) {
    cols_to_use <- value_cols
  }
  
  df_subset <- df %>% select(all_of(cols_to_use))
  
  # Define the summary statistics
  summary_stats <- list(
    count = ~ sum(!is.na(.x)),
    mean = ~ mean(.x, na.rm = TRUE),
    sd = ~ sd(.x, na.rm = TRUE),
    min = ~ min(.x, na.rm = TRUE),
    q25 = ~ quantile(.x, 0.25, na.rm = TRUE),
    q50 = ~ quantile(.x, 0.50, na.rm = TRUE),
    q75 = ~ quantile(.x, 0.75, na.rm = TRUE),
    max = ~ max(.x, na.rm = TRUE)
  )
  
  # Perform grouping and summarize
  if (!is.null(group_cols)) {
    result <- df_subset %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(across(all_of(value_cols), summary_stats, .names = "{.col}_{.fn}"), .groups = "drop")
  } else {
    result <- df_subset %>%
      summarise(across(all_of(value_cols), summary_stats, .names = "{.col}_{.fn}"))
  }
  
  return(result)
}   


#Call the function on the data
summary <- summary_table(data, value_cols = variables_to_explore, group_cols = grouping_variables)

#Option to write the summary table as a csv file
write_csv(summary, "summary_table.csv")

###########################################################################################################
#Plot boxplots for each column within each level of grouping specified
###########################################################################################################

plot_separate_boxplots <- function(df, value_cols, group_cols=NULL) {
  
  #' Returns a plot for each column in the value_col, 
  #' with boxplots grouped by levels of each variable in the group_col,
  #' with independent axes for each plot.

  # Loop through each value column to generate and save independent plots
  for (col in value_cols) {
    #Will run if group_cols is NULL or empty
    if (is.null(group_cols)) {
      print(paste0(col))
      # Create a unique filename
      filename <- paste0("boxplot_", col, ".pdf")
      pdf(filename, width = 6, height = 6)
      boxplot(
              data = df,
              x=data[[col]],
              main = paste("Distribution of", col),
              xlab = paste("Entire dataset"),
              ylab = paste(col),
              col = "lightblue",
              border = "darkblue"
      )
      dev.off() 
    }
    else { 
      #Will run if one or more variables are specified in group_cols
      # Create the right-hand side (RHS) of the formula: interaction(col1, col2, ...)
      rhs <- paste0("interaction(", paste(group_cols, collapse = ", "), ", drop = TRUE, lex.order = TRUE)")
      # Create the full formula string: "value_col ~ interaction(group1, group2)"
      formula_str <- paste(col, "~", rhs)
      print(formula_str)
      group_cols_str <- paste(group_cols, collapse = ", ")
      # Convert string to a formula object
      formula <- as.formula(formula_str)
      # Create a unique filename
      filename <- paste0("boxplot_", col, "_grouped_by_", group_cols_str, ".pdf")
      pdf(filename, width = 6, height = 6)
      boxplot(formula,
        data = df,
        #x=data[[col]],
        main = paste("Distribution of", col),
        xlab = paste("Grouped by", group_cols_str),
        ylab = paste(col),
        col = "lightblue",
        border = "darkblue"
      )
      dev.off() 
    }
  
  }
}  

#Call the function on the data
plot_separate_boxplots(data, value_cols = variables_to_explore, group_cols = grouping_variables)

###########################################################################################################
#Plot histograms for each column within each level of grouping specified
###########################################################################################################

plot_separate_histograms <- function(df, group_cols, value_cols, breaks = "Sturges") {

  #' Returns a plot for each column in the value_col, 
  #' with histograms grouped by levels of each variable in the group_col,
  #' with independent axes for each plot.
  
  if (is.null(group_cols)) {
  
  # Loop through each value column to create independent plots
    for (col in value_cols) {
      filename <- paste0("histogram_", col, ".pdf")
      pdf(filename, width = 6, height = 6)
      # Generate the histogram
      hist(data = df,
          x=data[[col]],
          main = paste(col),
          xlab = col,
          ylab = "Frequency",
          col = "lightblue",
          border = "darkblue",
          breaks = breaks
      )
      dev.off()
    }
    } else {
      
      # Create the interaction factor for all grouping variables
      # Loop through each unique group combination (cluster)
      # Loop through each value column to create independent plots
      for (col in value_cols) {
        group_factor <- interaction(df[group_cols], drop = TRUE, lex.order = TRUE)
        levels_list <- levels(group_factor)
        group_cols_str <- paste(group_cols, collapse = ", ")
        filename <- paste0("histogram_", col, "_grouped_by_", group_cols_str, ".pdf")
        pdf(filename, width = 6, height = 6)
        for (lvl in levels_list) {
        # Subset the data for the current group level
        subset_data <- df[[col]][group_factor == lvl]
      
        # Generate the histogram
        hist(subset_data,
            main = paste(col, "|", lvl),
            xlab = col,
            ylab = "Frequency",
            col = "lightblue",
            border = "darkblue",
            breaks = breaks
        )
        }
      dev.off()
    }
    }
}  

plot_separate_histograms(data, group_cols = grouping_variables, value_cols = variables_to_explore)

###########################################################################################################
#Plot a correlation matrix or matrices
###########################################################################################################


correlation_heatmap <- function(data, value_cols, group_cols = NULL) {
  
  #' Plots a heatmap of a correlation matrix for each unique combination of grouping variables.
  #'  If group_cols is NULL, it plots a single heatmap for the entire dataframe.
    
  #'  Parameters:
  #'  df (DataFrame): The input dataframe.
  #'  value_cols (list): List of column names to calculate correlations for.
  #'  group_cols (list or None): List of column names to group by, or NULL for the entire dataset.
  #'  figsize (tuple): Size of the figure.

  # Validate inputs
  if (!all(value_cols %in% names(data))) {
    stop("All 'value_cols' must exist in the dataset.")
  }
  
  # Select relevant columns
  cols_to_use <- c(value_cols, if (!is.null(group_cols)) group_cols)
  df_sub <- data[, cols_to_use, drop = FALSE]
  
  # Remove rows with NA in value columns
  df_sub <- df_sub[complete.cases(df_sub[, value_cols, drop = FALSE]), ]
  
  if (is.null(group_cols)) {
    # --- Case 1: No Grouping (Single Heatmap) ---
    corr_mat <- round(cor(df_sub[, value_cols, drop = FALSE]), 2)
    melted_corr <- melt(corr_mat)
    
    filename <- paste0("correlation_heatmap_entire_dataset.pdf")
    pdf(filename, width = 10, height = 10)
    
    print(ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = value), size = 4, color = "black") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limit = c(-1, 1), space = "Lab",
                           name = "Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10),
            axis.text.y = element_text(size=10),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank()) +
      ggtitle("Correlation Heatmap of the Entire Dataset"))
    
      dev.off()
    
  } else {
    # --- Case 2: With Grouping (Faceted Heatmaps) ---
    
    group_factor <- interaction(df_sub[group_cols], drop = TRUE, lex.order = TRUE)
    levels_list <- levels(group_factor)
    n_levels <- length(levels_list)
    
    # Calculate correlation per group
    processed_data <- df_sub %>%
      group_by(across(all_of(group_cols))) %>%
      group_modify(~ {
        if (nrow(.x) < 2) return(NULL)
        c_mat <- round(cor(.x[, value_cols, drop = FALSE]), 2)
        m_mat <- melt(c_mat)
        return(as_tibble(m_mat))
      }) %>%
      ungroup()
    
    group_cols_str <- paste(group_cols, collapse=", ")
    filename <- paste0("correlation_heatmap_grouped_by_", group_cols_str, ".pdf")
    pdf(filename, width = 10, height = 10*n_levels)
    
    # Construct the plot
    print(ggplot(processed_data, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = value), size = 3) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limit = c(-1, 1),
                           name = "Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10),
            axis.text.y = element_text(size=10),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            strip.background = element_rect(fill = "lightgray"),
            strip.text = element_text(face = "bold")) +
      ggtitle("Correlation Heatmap by Group") +
      facet_wrap(group_cols, scales = "free", ncol=1))
    
      dev.off()
  }
}     

correlation_heatmap(data, group_cols = grouping_variables, value_cols = variables_to_explore)

###########################################################################################################
#Generate scatterplots of pairs of variables fit with polynomial functions
###########################################################################################################

scatterplots_with_polynomial_fit <- function(data, 
                               value_cols = NULL, 
                               group_cols = NULL, 
                               degree = 1, 
                               n_points = 100, 
                               theme_color = "blue") {
  
  
  #' Plots scatter plots with polynomial fit lines for all unique column pairs,
  #'separated by unique combinations of multiple grouping columns.
    
  #'Parameters:
  #' df: DataFrame containing numeric data and grouping columns.
  #' value_cols (list): List of column names to calculate correlations for.
  #' group_cols (list or None): column names to group by.
  #' degree: int, degree of the polynomial fit (default=1 for linear).
  
  # Input Validation & Column Selection
  if (!is.data.frame(data)) stop("Input 'data' must be a dataframe.")
  
  # If cols is NULL, select all numeric columns
  if (is.null(value_cols)) {
    value_cols <- names(data)[sapply(data, is.numeric)]
    if (length(value_cols) < 2) stop("Dataframe must have at least two numeric columns for plotting.")
  } else {
    # Validate specified columns exist and are numeric
    if (!all(value_cols %in% names(data))) stop("Some specified columns are not in the dataframe.")
    if (!all(sapply(data[value_cols], is.numeric))) stop("All specified 'cols' must be numeric.")
  }

  
  # Generate Unique Pairs
  pairs_list <- combn(value_cols, 2, simplify = FALSE)
  
  if (length(pairs_list) == 0) stop("Not enough columns to create pairs.")
  
  # Define Plotting Logic
  plot_pair <- function(df_subset, x_var, y_var, group_label = NULL) {
    # Remove rows with NA in x or y
    clean_data <- df_subset[!is.na(df_subset[[x_var]]) & !is.na(df_subset[[y_var]]), ]
    
    if (nrow(clean_data) < degree + 1) {
      warning(paste("Skipping plot for", x_var, "vs", y_var, ": Insufficient data points for degree", degree))
      return()
    }
    
    # Create Title
    main_title <- paste(x_var, "vs", y_var)
    if (!is.null(group_label)) {
      main_title <- paste(main_title, "| Group:", group_label)
    }
    
    # Base Scatter Plot
    plot(clean_data[[x_var]], clean_data[[y_var]], 
         main = main_title, 
         xlab = x_var, ylab = y_var,
         pch = 19, col = rgb(0, 0, 0, 0.4), cex = 1.2)
    
    # Fit Polynomial Model
    # Formula construction: y ~ poly(x, degree, raw=TRUE)
    form <- as.formula(paste(y_var, "~ poly(", x_var, ",", degree, ", raw=TRUE)"))
    model <- lm(form, data = clean_data)
    
    # Generate Smooth Curve Data
    x_seq <- seq(min(clean_data[[x_var]]), max(clean_data[[x_var]]), length.out = n_points)
    pred_data <- data.frame(x_val = x_seq)
    names(pred_data) <- x_var
    
    y_pred <- predict(model, newdata = pred_data)
    
    # Add Fit Line
    lines(x_seq, y_pred, col = theme_color, lwd = 2)
    
    # Optional: Add R-squared to plot
    r_sq <- round(summary(model)$r.squared, 3)
    legend("topleft", legend = paste("R² =", r_sq), bty = "n", cex = 0.8)
  }
  
  # Create the plots
  if (is.null(group_cols)) {
    # No grouping: Generate a single plot
    
    for (pair in pairs_list) {
      filename <- paste0("scatterplot_of_", pair[1], "_and_", pair[2], ".pdf")
      pdf(filename, width = 6, height = 6)
      plot_pair(data, pair[1], pair[2])
      dev.off()
    }
  } else {
    # Grouping: Split data by unique combinations of group_cols
    # Create a unique ID for each group combination
    data$group_id <- interaction(data[group_cols], drop = TRUE)
    unique_groups <- unique(data$group_id)
    
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    
    for (g in unique_groups) {
      subset_df <- data[data$group_id == g, ]
      
      # Create label string for the group and filename for the plots
      g_label <- paste(names(group_cols), "=", unlist(subset_df[1, group_cols]), collapse = ", ")
      group_cols_str <- paste(group_cols, collapse=", ")
      #filename <- paste0("scatterplots_of_", pairs_list[1], "_and_", pairs_list[2], "_grouping_", g_label, ".pdf")
      filename <- paste0("scatterplots_grouped_as", g_label, ".pdf")
      pdf(filename, width = 6, height = 6)
      
      for (pair in pairs_list) {
        plot_pair(subset_df, pair[1], pair[2], group_label = g_label)
      }
      dev.off()
    }
  }
}      

#Call the function on the data

# Example Usage:

# 1. Grouped by multiple columns:
# scatterplots_with_polynomial_fit(df=data, value_cols=variables_to_explore, group_cols=['study', 'experiment', 'set_size'])
#
# 2. Entire dataset (no grouping):
# scatterplots_with_polynomial_fit(df=data, value_cols=variables_to_explore, group_cols=NULL)  

# 3. Entire dataset (no value cols or grouping variables specified):
# scatterplots_with_polynomial_fit(df=data, value_cols=NULL, group_cols=NULL)  

scatterplots_with_polynomial_fit(data, value_cols = variables_to_explore, group_cols = grouping_variables, degree = 5)

