##----------------------------------------------------------------------------------------------------##
## Categorical Variables: Chi-Square Independence & Homogeneity Test Package
##----------------------------------------------------------------------------------------------------##

Run_Categorical_Tests <- function(df) {
  if (!is.data.frame(df)) {
    stop("Please enter a dataframe")
  }
  
  # 1. Remind Alpha & Beta from Global Env
  alpha <- if (exists("global_alpha")) get("global_alpha", envir = .GlobalEnv) else 0.05
  beta <- if (exists("global_beta")) get("global_beta", envir = .GlobalEnv) else 0.20
  
  cat("\n==================================================\n")
  cat(" Categorical Chi-Square Tests (Homogeneity & Independence)\n")
  cat("==================================================\n")
  cat(sprintf("[Reminder] Current Alpha: %g | Current Beta: %g\n", alpha, beta))
  cat("\nNOTE: These tests mandate Categorical/Discrete variables (e.g., characters, factors, or grouped integers).\n\n")
  
  # 2. Test Approach Selection
  mode_choice <- select.list(
    c("1. Inner Category (Goodness of Fit / Homogeneity for precisely ONE variable)",
      "2. Inter-Variable (Test of Statistical Independence between TWO discrete variables)"),
    multiple = FALSE,
    title = "Which Chi-square test logic do you want to calculate?"
  )
  
  if (mode_choice == "" || is.na(mode_choice)) {
    cat("No validation logic selected. Exiting.\n")
    return(invisible(NULL))
  }
  
  all_cols <- colnames(df)
  
  cat("\n--------------------------------------------------\n")
  
  # ===============================================
  # OPTION 1: INNER CATEGORY (Goodness of Fit)
  # ===============================================
  if (grepl("Inner Category", mode_choice)) {
    
    var_selected <- select.list(
      all_cols, 
      multiple = FALSE, 
      title = "Select EXACTLY ONE categorical variable to test internal homogeneity:"
    )
    
    if (var_selected == "" || is.na(var_selected)) return(invisible(NULL))
    
    cat(sprintf("\n[Reminder] Selected Variable: %s\n", var_selected))
    cat(sprintf("           Variable Type    : %s\n", class(df[[var_selected]])[1]))
    cat("  -> Logic: Chi-Square Goodness of Fit Test\n")
    cat("  -> Null Hypothesis: Core categories map equal frequencies (Completely Homogeneous)\n")
    
    # Compile safe explicit frequencies
    data_vec <- na.omit(df[[var_selected]])
    freq_table <- table(data_vec)
    
    cat("\n=> Observed Dimensional Frequencies:\n")
    print(freq_table)
    
    if (length(freq_table) < 2) {
      cat("\nError: The mapped variable physically must contain at least 2 distinct categories to calculate homogeneity. Operation aborted.\n")
      return(invisible(NULL))
    }
    
    # Execute backend math
    res <- tryCatch(chisq.test(freq_table), error = function(e) e)
    if (!inherits(res, "error")) {
      cat(sprintf("\n   Chi-squared metric : %.4f,  calculated df: %d\n", res$statistic, res$parameter))
      cat(sprintf("   P-value            : %.4g\n", res$p.value))
      
      if (res$p.value < alpha) {
         cat(sprintf("\n   Conclusion: Reject Null (The inner category distributions for '%s' are significantly divergent / Non-Homogeneous at alpha=%g)\n", var_selected, alpha))
      } else {
         cat(sprintf("\n   Conclusion: Failed to reject Null (The frequencies are statistically homogeneous/uniformly distributed at alpha=%g)\n", alpha))
      }
    } else {
      cat("Error running intrinsic Chi-Square alignment:", conditionMessage(res), "\n")
    }
    
  # ===============================================
  # OPTION 2: INTER-VARIABLE (Independence constraint)
  # ===============================================
  } else {
    
    vars_selected <- select.list(
      all_cols, 
      multiple = TRUE, 
      title = "Select EXACTLY TWO categorical variables to test specific independence:"
    )
    
    if (length(vars_selected) != 2) {
      cat(sprintf("\nError: You selected %d variables. You must select exactly 2 for a valid inter-variable matrix mapping. Exiting.\n", length(vars_selected)))
      return(invisible(NULL))
    }
    
    v1 <- vars_selected[1]
    v2 <- vars_selected[2]
    
    cat(sprintf("\n[Reminder] Selected Variables:\n  1. %s (Type: %s)\n  2. %s (Type: %s)\n", 
                v1, class(df[[v1]])[1], v2, class(df[[v2]])[1]))
    cat("  -> Logic: Chi-Square Test of Functional Independence\n")
    cat("  -> Null Hypothesis: Variables act completely independent of each other (No specific intrinsic association)\n")
    
    # Safe pairwise overlap only
    comp_env <- complete.cases(df[, c(v1, v2)])
    d1 <- df[comp_env, v1]
    d2 <- df[comp_env, v2]
    
    freq_table <- table(d1, d2)
    cat("\n=> Resulting Contingency Output Matrix:\n")
    print(freq_table)
    
    if (nrow(freq_table) < 2 || ncol(freq_table) < 2) {
      cat("\nError: To validate mathematical independence, both chosen boundaries must securely hold at least 2 distinct variant categories.\n")
      return(invisible(NULL))
    }
    
    # Calculate constraint natively tracking warning logic for low Expected Frequencies
    res_warn <- NULL
    res <- tryCatch(chisq.test(freq_table), warning = function(w) w, error = function(e) e)
    
    if (inherits(res, "warning")) {
      res_warn <- conditionMessage(res)
      res <- suppressWarnings(chisq.test(freq_table))
    }
    
    if (!inherits(res, "error")) {
      
      if (!is.null(res_warn)) {
         cat(sprintf("\n   [Mathematical Note]: %s\n   (The expected underlying frequencies in some matrix cells are extremely low. Defaulting to standard chi-square approximation, but calculating Fisher's Exact safety net shortly).\n", res_warn))
      }
      
      cat(sprintf("\n   Chi-squared metric : %.4f,  calculated df: %d\n", res$statistic, res$parameter))
      cat(sprintf("   P-value            : %.4g\n", res$p.value))
      
      if (res$p.value < alpha) {
        cat(sprintf("\n   Conclusion: Reject Null (Statistically solid dependency/association exists securely between '%s' and '%s' at alpha bounds=%g)\n", v1, v2, alpha))
      } else {
        cat(sprintf("\n   Conclusion: Failed to reject Null (Variables '%s' and '%s' operate cleanly and are statistically independent at alpha bounds=%g)\n", v1, v2, alpha))
      }
      
      # Automatically run Fisher's exact contingency fallbacks if table mapping warns
      if (!is.null(res_warn) && nrow(freq_table) <= 5 && ncol(freq_table) <= 5) {
        cat("\n=> Triggering Fisher's Exact Boolean Test fallback protocol due to matrix instability concerns...\n")
        fish_res <- tryCatch(fisher.test(freq_table, simulate.p.value=TRUE), error=function(e) e)
        
        if (!inherits(fish_res, "error")) {
           cat(sprintf("   Fisher Exact P-value: %.4g\n", fish_res$p.value))
           if (fish_res$p.value < alpha) {
             cat("   Fisher Conclusion: Reject Null Hypothesis (Dependency explicitly verified)\n")
           } else {
             cat("   Fisher Conclusion: Failed to reject Null (Independence effectively verified)\n")
           }
        } else {
           cat("   [Fisher fallback aborted]: Matrices size constraints exceeded memory handling boundary.\n")
        }
      }
      
    } else {
      cat("Error firing backend Independence testing structures:", conditionMessage(res), "\n")
    }
  }
  
  cat("\n==================================================\n")
  cat(" Categorical Validation Phase Completed.\n")
  cat("==================================================\n")
}
