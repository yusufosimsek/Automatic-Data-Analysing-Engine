##----------------------------------------------------------------------------------------------------##
## Correlation Tests Package
##----------------------------------------------------------------------------------------------------##

Run_Correlation_Tests <- function(df) {
  if (!is.data.frame(df)) {
    stop("Please enter a dataframe")
  }
  
  # 1. Remind Alpha & Beta from Global Env
  alpha <- if (exists("global_alpha")) get("global_alpha", envir = .GlobalEnv) else 0.05
  beta <- if (exists("global_beta")) get("global_beta", envir = .GlobalEnv) else 0.20
  
  cat("\n==================================================\n")
  cat(" Correlation Tests\n")
  cat("==================================================\n")
  cat(sprintf("[Reminder] Current Alpha: %g | Current Beta: %g\n", alpha, beta))
  
  # 2. Variable Selection
  valid_cols <- c()
  for (col_name in colnames(df)) {
    if (is.numeric(df[[col_name]])) {
      valid_cols <- c(valid_cols, col_name)
    }
  }
  
  if (length(valid_cols) < 2) {
    cat("\nNot enough numeric variables to perform correlation (need at least 2).\n")
    return(invisible(NULL))
  }
  
  selected_vars <- select.list(
    valid_cols, 
    multiple = TRUE, 
    title = "Select TWO OR MORE variables for correlation testing:"
  )
  
  if (length(selected_vars) < 2) {
    cat("\nYou must select at least two variables. Exiting.\n")
    return(invisible(NULL))
  }
  
  # 3. Remind Variable Types
  cat("\n[Reminder] Selected Variables & Types:\n")
  for(v in selected_vars) {
    cat(sprintf("  - %s : %s\n", v, class(df[[v]])[1]))
  }
  
  # 4. Remind Distribution Approaching 
  cat("\n[Reminder] Distribution Approaching for Correlation:\n")
  cat("  -> Parametric   : (Assumes Normality) Opt for 'Pearson'.\n")
  cat("  -> Non-Parametric: (Non-Normal / Ordinal) Opt for 'Spearman' or 'Kendall'.\n\n")
  
  # 5. Ask Which Correlation Test
  test_choice <- select.list(
    c("Pearson (Parametric - Linear Relationship)", 
      "Spearman (Non-Parametric - Monotonic Relationship)", 
      "Kendall (Non-Parametric - Rank Concordance)"),
    multiple = FALSE,
    title = "which correlation tests do you want?"
  )
  
  if (test_choice == "" || is.na(test_choice)) {
    cat("No correlation test selected. Exiting.\n")
    return(invisible(NULL))
  }
  
  # Extract the name of the test for the R method parameter
  method_name <- tolower(strsplit(test_choice, " ")[[1]][1])
  
  cat("\n--------------------------------------------------\n")
  cat(sprintf(" RUNNING %s CORRELATION TESTS\n", toupper(method_name)))
  cat("--------------------------------------------------\n")
  
  # Execute combinations
  pairs <- combn(selected_vars, 2, simplify = FALSE)
  
  for (p in pairs) {
    v1_name <- p[1]
    v2_name <- p[2]
    
    # Isolate valid paired data to prevent NA mismatch errors
    comp <- complete.cases(df[, c(v1_name, v2_name)])
    v1_data <- as.numeric(df[comp, v1_name])
    v2_data <- as.numeric(df[comp, v2_name])
    
    cat(sprintf("\n=> Testing pair: [%s] and [%s]\n", v1_name, v2_name))
    
    if (length(v1_data) < 3) {
      cat("   Error: Not enough valid paired observations (minimum 3 required).\n")
      next
    }
    
    # Run the dynamic correlation test based on user choice
    res <- tryCatch(
      suppressWarnings(cor.test(v1_data, v2_data, method = method_name, conf.level = 1 - alpha)), 
      error = function(e) e
    )
    
    if (!inherits(res, "error")) {
      
      # For Kendall/Spearman, the statistic is returned differently or with warnings
      if (!is.null(res$statistic)) {
         cat(sprintf("   Test Statistic : %.4f\n", as.numeric(res$statistic)))
      }
      
      cat(sprintf("   Correlation (r): %.4f\n", as.numeric(res$estimate)))
      cat(sprintf("   P-value        : %.4g\n", as.numeric(res$p.value)))
      
      # Confidence intervals are generally only output natively for Pearson in base R
      if (!is.null(res$conf.int)) {
        cat(sprintf("   %g%% CI         : [%.4f, %.4f]\n", (1-alpha)*100, res$conf.int[1], res$conf.int[2]))
      }
      
      # Make conclusion based on assigned alpha
      if (!is.na(res$p.value) && res$p.value < alpha) {
        cat("   Conclusion     : Reject Null Hypothesis (Statistically significant correlation exists!)\n")
      } else {
        cat("   Conclusion     : Failed to reject Null Hypothesis (No statistically significant correlation)\n")
      }
      
    } else {
      cat("   Error running test:", conditionMessage(res), "\n")
    }
  }
  
  cat("\n==================================================\n")
  cat(" Correlation Testing Completed.\n")
  cat("==================================================\n")
}
