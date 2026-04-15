##----------------------------------------------------------------------------------------------------##
## Normality Test Package
##----------------------------------------------------------------------------------------------------##

Test_Normality <- function(df) {
  if (!is.data.frame(df)) {
    stop("Please enter a dataframe")
  }
  
  # Find numeric and date columns
  valid_cols <- c()
  for (col_name in colnames(df)) {
    col_data <- df[[col_name]]
    if (is.numeric(col_data) || inherits(col_data, "Date") || inherits(col_data, "POSIXt")) {
      valid_cols <- c(valid_cols, col_name)
    }
  }
  
  if (length(valid_cols) == 0) {
    cat("\nNo numeric or date variables found in the dataframe for normality testing.\n")
    return(invisible(NULL))
  }
  
  # Ask end-user which variables to test
  cat("\n==================================================\n")
  cat(" Normality Test Selection\n")
  cat("==================================================\n")
  
  selected_vars <- select.list(
    valid_cols, 
    multiple = TRUE, 
    title = "Which variables do you want to make test of normality?"
  )
  
  if (length(selected_vars) == 0 || selected_vars[1] == "") {
    cat("\nNo variables selected. Exiting normality test.\n")
    return(invisible(NULL))
  }
  
  results <- list()
  
  cat("\n==================================================\n")
  cat(" Normality Test Results (Shapiro-Wilk)\n")
  cat("==================================================\n")
  
  for (var in selected_vars) {
    cat("\nVariable: [", var, "]\n", sep="")
    x <- df[[var]]
    
    # If date, convert to numeric before normality testing
    if (inherits(x, "Date") || inherits(x, "POSIXt")) {
      x <- as.numeric(x)
      cat("  *(Note: Date variable converted to numeric for the test)\n")
    }
    
    x_clean <- x[!is.na(x)]
    
    if (length(x_clean) < 3) {
      cat("  -> Not enough valid observations (need at least 3). Skipping.\n")
      results[[var]] <- NA
      next
    }
    
    # check for identical values which break shapiro.test or zero variance
    if (length(unique(x_clean)) == 1) {
      cat("  -> Variable has identical values (zero variance). Skipping.\n")
      results[[var]] <- NA
      next
    }
    
    # shapiro.test handles max 5000 sample size
    sampled <- FALSE
    if (length(x_clean) > 5000) {
      cat("  -> Sample size > 5000. Using a random sample of 5000 for Shapiro-Wilk test.\n")
      x_clean <- sample(x_clean, 5000)
      sampled <- TRUE
    }
    
    test_res <- tryCatch({
      shapiro.test(x_clean)
    }, error = function(e) {
      cat("  -> Error running Shapiro-Wilk test:", conditionMessage(e), "\n")
      return(NULL)
    })
    
    if (!is.null(test_res)) {
      cat(sprintf("  -> W-statistic : %.4f\n", test_res$statistic))
      cat(sprintf("  -> p-value     : %.4g\n", test_res$p.value))
      
      if (test_res$p.value < 0.05) {
        cat("  -> Conclusion  : Distribution significantly deviates from normality (p < 0.05).\n")
      } else {
        cat("  -> Conclusion  : Failed to reject normality (p >= 0.05). Distribution may be normal.\n")
      }
      
      results[[var]] <- list(
        W_statistic = test_res$statistic, 
        p_value = test_res$p.value, 
        sampled = sampled
      )
    }
  }
  
  cat("\nNormality testing completed.\n")
  return(invisible(results))
}
