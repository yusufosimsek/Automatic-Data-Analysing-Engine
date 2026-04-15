library(moments)
library(ggplot2)

##----------------------------------------------------------------------------------------------------##
## STEP 1: Main Analysis Function
##----------------------------------------------------------------------------------------------------##

Analyze_Dataframe <- function(df) {
  if (!is.data.frame(df)) {
    stop("Please enter a dataframe")
  }
  
  # Ensure all columns have names
  for (i in seq_along(df)) {
    if (is.null(colnames(df)[i]) || colnames(df)[i] == "" || is.na(colnames(df)[i])) {
      colnames(df)[i] <- paste0("Variable_", i)
    }
  }
  
  results <- list()
  
  # Start interactive iteration for every variable
  for (col_name in colnames(df)) {
    cat("\n==================================================\n")
    cat(" Analyzing: ", col_name, "\n")
    cat("==================================================\n")
    
    variable_data <- df[[col_name]]
    cat("First few values: ", paste(head(variable_data, 3), collapse = ", "), "\n")
    
    # USER CHOOSES THE TYPE INTERACTIVELY
    type_choice <- menu(c("Numeric", "Categoric (Factor)", "Date", "Other"), 
                        title = paste("What is the data type for [", col_name, "]?"))
    
    if (type_choice == 1) { # Numeric
      # Force numeric conversion to be safe
      variable_data <- as.numeric(as.character(variable_data))
      results[[col_name]] <- Descriptive_Statistic(variable_data, col_name)
      
    } else if (type_choice == 2) { # Categoric
      results[[col_name]] <- Categoric_Statistic(variable_data, col_name)
      
    } else if (type_choice == 3) { # Date
      # Force Date conversion
      variable_data <- as.POSIXct(variable_data)
      results[[col_name]] <- Date_Statistic(variable_data, col_name)
      
    } else {
      cat("Skipping variable...\n")
    }
  }
  
  cat("\nAll analyses have been completed.\n")
  return(invisible(results))
}

##----------------------------------------------------------------------------------------------------##
## STEP 2: Numeric Function (With Interactive Mean and Graph Selection)
##----------------------------------------------------------------------------------------------------##

Descriptive_Statistic <- function(x, col_name) {
  x_clean <- x[!is.na(x)]
  
  # 1. Interactive Mean Selection
  choices <- c("Arithmetic", "Trimmed (%10)", "Exponential (Geometric)", "Harmonic")
  selections <- select.list(choices, multiple = TRUE, title = paste("Select Mean Types for", col_name))
  
  mean_results <- list()
  if ("Arithmetic" %in% selections) mean_results$Arithmetic <- mean(x_clean)
  if ("Trimmed (%10)" %in% selections) mean_results$Trimmed <- mean(x_clean, trim = 0.1)
  if ("Exponential (Geometric)" %in% selections) {
    mean_results$Geometric <- if(any(x_clean <= 0)) "Error: Values <= 0" else exp(mean(log(x_clean)))
  }
  if ("Harmonic" %in% selections) {
    mean_results$Harmonic <- if(any(x_clean <= 0)) "Error: Values <= 0" else 1 / mean(1 / x_clean)
  }
  
  mean_val <- mean(x_clean)
  # Summary Stats
  res <- list(
    Missing_Count  = sum(is.na(x)),
    Selected_Means = mean_results,
    Variance       = var(x_clean),
    Std_Deviation  = sd(x_clean),
    CV_Ratio       = if(mean_val == 0) NA else (sd(x_clean) / mean_val),
    Skewness       = skewness(x_clean),
    Kurtosis       = kurtosis(x_clean),
    Median         = median(x_clean),
    Mode           = Mode_Types(x_clean),
    IQR            = IQR(x_clean),
    Quartiles      = quantile(x_clean)
  )
  print(res)
  
  # 2. Interactive Graph Selection
  graph_list <- c("Histogram", "Box-Plot", "Violin-Plot", "Density-Plot", "QQ-Plot")
  selected_graphs <- select.list(graph_list, multiple = TRUE, title = paste("Select Graphs for", col_name))
  
  for (g in selected_graphs) {
    p <- ggplot(data.frame(val = x_clean), aes(x = val))
    if (g == "Histogram") {
      print(p + geom_histogram(fill = "steelblue", color = "white") + labs(title = paste(col_name, "Histogram")))
    } else if (g == "Box-Plot") {
      print(ggplot(data.frame(val = x_clean), aes(y = val)) + geom_boxplot(fill = "tomato") + labs(title = paste(col_name, "Box-Plot")))
    } else if (g == "Violin-Plot") {
      print(ggplot(data.frame(val = x_clean), aes(x = "", y = val)) + geom_violin(fill = "gold") + labs(title = paste(col_name, "Violin Plot")))
    } else if (g == "Density-Plot") {
      print(p + geom_density(fill = "lightgreen", alpha = 0.5) + labs(title = paste(col_name, "Density Plot")))
    } else if (g == "QQ-Plot") {
      print(ggplot(data.frame(val = x_clean), aes(sample = val)) + stat_qq() + stat_qq_line() + labs(title = paste(col_name, "QQ Plot")))
    }
  }
  return(res)
}

##----------------------------------------------------------------------------------------------------##
## STEP 3: Categoric and Date Helper Functions
##----------------------------------------------------------------------------------------------------##

Categoric_Statistic <- function(x, col_name) {
  x_clean <- x[!is.na(x)]
  freq_table <- table(x_clean)
  perc_table <- prop.table(freq_table) * 100
  
  final_df <- data.frame(Count = as.vector(freq_table), 
                         Percentage = paste0(round(as.vector(perc_table), 2), "%"), 
                         row.names = names(freq_table))
  print(final_df)
  
  if (menu(c("Yes", "No"), title = paste("Show Bar Plot for", col_name, "?")) == 1) {
    print(ggplot(data.frame(x_clean), aes(x = x_clean, fill = x_clean)) + 
            geom_bar() + labs(title = paste(col_name, "Distribution")) + theme_minimal())
  }
  return(invisible(final_df))
}

Date_Statistic <- function(x, col_name) {
  cat("\n>>> Date Analysis for", col_name, ":\n")
  x_clean <- x[!is.na(x)]
  
  # Summary Stats (Including Mean and Median for Time)
  res <- list(
    Min         = min(x_clean),
    Max         = max(x_clean),
    Range       = difftime(max(x_clean), min(x_clean)),
    Mean        = as.POSIXct(mean(as.numeric(x_clean)), origin = "1970-01-01"),
    Median      = as.POSIXct(median(as.numeric(x_clean)), origin = "1970-01-01"),
    Quartiles   = as.POSIXct(quantile(as.numeric(x_clean)), origin = "1970-01-01")
  )
  print(res)
  
  # Interactive Graph Selection
  graph_choice <- menu(c("Time Series Histogram", "Temporal Density Plot", "No"), 
                       title = paste("Select Graph for", col_name))
  
  if (graph_choice == 1) {
    print(ggplot(data.frame(d = x_clean), aes(x = d)) + 
            geom_histogram(fill = "darkgreen", color = "white") + 
            labs(title = paste(col_name, "Timeline")) + theme_minimal())
  } else if (graph_choice == 2) {
    print(ggplot(data.frame(d = x_clean), aes(x = d)) + 
            geom_density(fill = "cyan", alpha = 0.4) + 
            labs(title = paste(col_name, "Density over Time")) + theme_minimal())
  }
  
  return(res)
}

Mode_Types <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 0) return(NA)
  tab <- table(x)
  modes <- names(tab)[tab == max(tab)]
  return(paste(modes, collapse = ", "))
}