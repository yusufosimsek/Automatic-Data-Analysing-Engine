##----------------------------------------------------------------------------------------------------##
## Point Estimation and Confidence Interval Package
##----------------------------------------------------------------------------------------------------##

Estimate_Point_and_CI <- function(df) {
  if (!is.data.frame(df)) {
    stop("Please enter a dataframe")
  }
  
  # Find numeric columns
  valid_cols <- c()
  for (col_name in colnames(df)) {
    if (is.numeric(df[[col_name]])) {
      valid_cols <- c(valid_cols, col_name)
    }
  }
  
  if (length(valid_cols) == 0) {
    cat("\nNo numeric variables found in the dataframe for estimation.\n")
    return(invisible(NULL))
  }
  
  # Ask end-user which variables to estimate
  cat("\n==================================================\n")
  cat(" Estimation and Confidence Intervals Selection\n")
  cat("==================================================\n")
  
  selected_vars <- select.list(
    valid_cols, 
    multiple = TRUE, 
    title = "Which variables do you want to estimate and get CI for?"
  )
  
  if (length(selected_vars) == 0 || selected_vars[1] == "") {
    cat("\nNo variables selected. Exiting estimation.\n")
    return(invisible(NULL))
  }
  
  # Fetch alpha from global environment or default to 0.05
  # This uses the alpha determined in Alpha_Beta_Determiner.R
  alpha <- if (exists("global_alpha")) get("global_alpha", envir = .GlobalEnv) else 0.05
  conf_level <- 1 - alpha
  
  results <- list()
  
  for (var in selected_vars) {
    cat("\n==================================================\n")
    cat(" Analyzing Variable: [", var, "]\n", sep="")
    
    x <- df[[var]]
    x_clean <- x[!is.na(x)]
    n <- length(x_clean)
    
    if (n < 2) {
      cat("  -> Not enough valid observations (need at least 2). Skipping.\n")
      results[[var]] <- NA
      next
    }
    
    # Ask the approach
    cat("\n  *Note: Users can choose the approach based on their preference or CLT,\n")
    cat("         regardless of previous normality test results.\n")
    approach <- select.list(
      c("Parametric (Assumes Normality / Uses Central Limit Theorem)", 
        "Non-Parametric (Distribution-free methods / Bootstrap)"),
      multiple = FALSE,
      title = paste("Which approach do you want for:", var, "?")
    )
    
    if (approach == "" || is.na(approach)) {
      cat("  -> No approach selected. Skipping.\n")
      next
    }
    
    if (grepl("Parametric", approach)) {
      cat("\n  [ Approach Selected ]: Parametric\n")
      
      # 1. Mean
      mean_est <- mean(x_clean)
      t_critical <- qt(1 - alpha/2, df = n - 1)
      se_mean <- sd(x_clean)/sqrt(n)
      mean_ci_lower <- mean_est - t_critical * se_mean
      mean_ci_upper <- mean_est + t_critical * se_mean
      
      # 2. Variance
      var_est <- var(x_clean)
      chi_lower_critical <- qchisq(1 - alpha/2, df = n - 1)
      chi_upper_critical <- qchisq(alpha/2, df = n - 1)
      var_ci_lower <- (n - 1) * var_est / chi_lower_critical
      var_ci_upper <- (n - 1) * var_est / chi_upper_critical
      
      # 3. Standard Deviation
      std_est <- sd(x_clean)
      std_ci_lower <- sqrt(var_ci_lower)
      std_ci_upper <- sqrt(var_ci_upper)
      
      cat(sprintf("  -> Mean       : %.4f | %g%% CI: [%.4f, %.4f]\n", mean_est, conf_level*100, mean_ci_lower, mean_ci_upper))
      cat(sprintf("  -> Variance   : %.4f | %g%% CI: [%.4f, %.4f]\n", var_est, conf_level*100, var_ci_lower, var_ci_upper))
      cat(sprintf("  -> Std Dev    : %.4f | %g%% CI: [%.4f, %.4f]\n", std_est, conf_level*100, std_ci_lower, std_ci_upper))
      
      results[[var]] <- list(
        Approach = "Parametric",
        Mean = c(Estimate = mean_est, CI_Lower = mean_ci_lower, CI_Upper = mean_ci_upper),
        Variance = c(Estimate = var_est, CI_Lower = var_ci_lower, CI_Upper = var_ci_upper),
        StdDev = c(Estimate = std_est, CI_Lower = std_ci_lower, CI_Upper = std_ci_upper)
      )
      
    } else {
      cat("\n  [ Approach Selected ]: Non-Parametric\n")
      
      # 1. Location Estimate (Using Wilcoxon for Pseudo-median)
      wilcox_res <- tryCatch({
        suppressWarnings(wilcox.test(x_clean, conf.int = TRUE, conf.level = conf_level))
      }, error = function(e) return(NULL))
      
      loc_est <- NA
      loc_ci_lower <- NA
      loc_ci_upper <- NA
      
      if (!is.null(wilcox_res) && !is.null(wilcox_res$conf.int)) {
        loc_est <- wilcox_res$estimate
        loc_ci_lower <- wilcox_res$conf.int[1]
        loc_ci_upper <- wilcox_res$conf.int[2]
        cat(sprintf("  -> Location (Pseudo-median): %.4f | %g%% CI: [%.4f, %.4f]\n", loc_est, conf_level*100, loc_ci_lower, loc_ci_upper))
      } else {
        # Fallback to simple Median
        loc_est <- median(x_clean)
        cat(sprintf("  -> Location (Median): %.4f | CI: [Not Available]\n", loc_est))
      }
      
      # 2 & 3. Variance and StdDev using Bootstrap for Non-Parametric approach
      cat("  -> (Calculating non-parametric variance & std CIs via Bootstrap...)\n")
      B <- 2000
      boot_vars <- numeric(B)
      for (i in 1:B) {
        boot_samp <- sample(x_clean, size = n, replace = TRUE)
        boot_vars[i] <- var(boot_samp)
      }
      
      var_est <- var(x_clean)
      var_ci <- quantile(boot_vars, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
      
      std_est <- sd(x_clean)
      
      cat(sprintf("  -> Variance   : %.4f | %g%% CI: [%.4f, %.4f] (Bootstrap)\n", var_est, conf_level*100, var_ci[1], var_ci[2]))
      cat(sprintf("  -> Std Dev    : %.4f | %g%% CI: [%.4f, %.4f] (Bootstrap)\n", std_est, conf_level*100, sqrt(var_ci[1]), sqrt(var_ci[2])))
      
      results[[var]] <- list(
        Approach = "Non-Parametric",
        Location = c(Estimate = loc_est, CI_Lower = loc_ci_lower, CI_Upper = loc_ci_upper),
        Variance = c(Estimate = var_est, CI_Lower = as.numeric(var_ci[1]), CI_Upper = as.numeric(var_ci[2])),
        StdDev = c(Estimate = std_est, CI_Lower = sqrt(as.numeric(var_ci[1])), CI_Upper = sqrt(as.numeric(var_ci[2])))
      )
    }
  }
  
  cat("\n==================================================\n")
  cat(" Estimation and Confidence Intervals Completed.\n")
  cat("==================================================\n")
  return(invisible(results))
}
