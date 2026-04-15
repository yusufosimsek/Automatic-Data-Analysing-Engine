##----------------------------------------------------------------------------------------------------##
## Outlier Detection and Handling Engine
##----------------------------------------------------------------------------------------------------##

Run_Outlier_Handling <- function(df) {
  if (!is.data.frame(df)) stop("Error: Please supply a valid dataframe object.")
  
  cat("\n==================================================\n")
  cat(" Outlier Detection and Handling Machine\n")
  cat("==================================================\n")
  
  repeat {
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) == 0) {
      cat("\n=> System Halt: No explicit numeric variables available for outlier assessment.\n")
      break
    }
    
    target_var <- select.list(
      c(num_cols, "Exit Outlier Engine"),
      multiple = FALSE,
      title = "Which mapped numeric variable do you want to scan for extreme outliers?"
    )
    
    if (target_var == "" || grepl("Exit", target_var) || is.na(target_var)) {
       cat("\n=> Extracting cleanly out of Outlier parameters.\n")
       break
    }
    
    cat(sprintf("\n=> Actively scanning structural boundaries for parameter: '%s'\n", target_var))
    orig_vec <- as.numeric(df[[target_var]])
    
    method <- select.list(
      c("1. Standard Boxplot IQR (Detects bounds dynamically utilizing relative Quartiles)", 
        "2. Strict Z-Score Threshold (Identifies bounds mathematically > 3 Standard Deviations)"),
      multiple = FALSE,
      title = "Which specific mathematical outlier detection formula should be rigorously enforced?"
    )
    
    if (method == "" || is.na(method)) next
    
    outlier_idx <- c()
    lower_bound <- NA
    upper_bound <- NA
    
    if (grepl("IQR", method)) {
       q1 <- quantile(orig_vec, 0.25, na.rm=TRUE)
       q3 <- quantile(orig_vec, 0.75, na.rm=TRUE)
       iqr_val <- q3 - q1
       lower_bound <- q1 - 1.5 * iqr_val
       upper_bound <- q3 + 1.5 * iqr_val
       outlier_idx <- which(orig_vec < lower_bound | orig_vec > upper_bound)
    } else {
       mean_val <- mean(orig_vec, na.rm=TRUE)
       sd_val <- sd(orig_vec, na.rm=TRUE)
       z_scores <- (orig_vec - mean_val) / sd_val
       outlier_idx <- which(abs(z_scores) > 3 & !is.na(z_scores))
       lower_bound <- mean_val - 3 * sd_val
       upper_bound <- mean_val + 3 * sd_val
    }
    
    num_outliers <- length(outlier_idx)
    
    cat("\n--------------------------------------------------\n")
    cat(sprintf("=> Lower acceptable mathematical bound computed : %.4f\n", lower_bound))
    cat(sprintf("=> Upper acceptable mathematical bound computed : %.4f\n", upper_bound))
    cat(sprintf("=> Total Extreme Physical Outliers Detected   : %d\n", num_outliers))
    cat("--------------------------------------------------\n")
    
    if (num_outliers == 0) {
      cat("=> Structural Check: No explicitly extreme outliers found violating bounds. Returning intact.\n")
      next
    }
    
    cat("\n[Network Warning]: Explicit Outlier variance threatens deep parametric bounds (Linear architectures).\n")
    action <- select.list(
      c("1. Capping (Winsorizing) - Force isolated outliers exactly to limit bounds structurally",
        "2. Nullification (NA) - Obliterate distinct extreme values into logically Missing states",
        "3. Row Deletion - Completely purge the total corresponding matrix rows globally",
        "4. Bypass System - Leave outliers definitively untouched"),
      multiple = FALSE,
      title = "How do you actively demand the framework to handle these broken array structures?"
    )
    
    if (grepl("Bypass", action) || action == "" || is.na(action)) {
      cat("\n=> Protocol bypassed natively. No physical data overwritten.\n")
      next
    }
    
    if (grepl("Capping", action)) {
      df[[target_var]][orig_vec < lower_bound & !is.na(orig_vec)] <- lower_bound
      df[[target_var]][orig_vec > upper_bound & !is.na(orig_vec)] <- upper_bound
      cat(sprintf("\n=> Output Success: Assigned strict Cap constraint over %d isolated variables mapping rigidly.\n", num_outliers))
    } else if (grepl("Nullification", action)) {
      df[[target_var]][outlier_idx] <- NA
      cat(sprintf("\n=> Output Success: Dissolved exactly %d extreme metrics into structural mathematical NAs explicitly.\n", num_outliers))
    } else if (grepl("Row Deletion", action)) {
      df <- df[-outlier_idx, ]
      cat(sprintf("\n=> Output Success: Completely expunged %d target cross-matrix rows containing the invalid parameters.\n", num_outliers))
    }
  }
  
  cat("\n==================================================\n")
  cat(" Outlier Engine cycling complete. Transporting optimized Array Matrix logically.\n")
  cat("==================================================\n\n")
  return(df)
}
