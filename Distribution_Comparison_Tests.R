##----------------------------------------------------------------------------------------------------##
## Distribution Comparison and Homogeneity Tests Package
##----------------------------------------------------------------------------------------------------##

Compare_Distributions <- function(df) {
  if (!is.data.frame(df)) {
    stop("Please enter a dataframe")
  }
  
  # Remind Alpha & Beta from Global Env
  alpha <- if (exists("global_alpha")) get("global_alpha", envir = .GlobalEnv) else 0.05
  beta <- if (exists("global_beta")) get("global_beta", envir = .GlobalEnv) else 0.20
  
  cat("\n==================================================\n")
  cat(" Distribution Comparison / Homogeneity Tests\n")
  cat("==================================================\n")
  cat(sprintf("[Reminder] User Alpha: %g | User Beta: %g\n", alpha, beta))
  
  # Select variables
  valid_cols <- c()
  for (col_name in colnames(df)) {
    if (is.numeric(df[[col_name]])) {
      valid_cols <- c(valid_cols, col_name)
    }
  }
  
  if (length(valid_cols) < 2) {
    cat("\nNot enough numeric variables to compare (need at least 2).\n")
    return(invisible(NULL))
  }
  
  selected_vars <- select.list(
    valid_cols, 
    multiple = TRUE, 
    title = "Select TWO OR MORE variables to compare:"
  )
  
  if (length(selected_vars) < 2) {
    cat("\nYou must select at least two variables. Exiting.\n")
    return(invisible(NULL))
  }
  
  k <- length(selected_vars)
  
  # Remind Variable Types
  cat("\n[Reminder] Selected Variables & Types:\n")
  for(v in selected_vars) {
    cat(sprintf("  - %s : %s\n", v, class(df[[v]])[1]))
  }
  
  cat("\n")
  # Ask for approach
  approach <- select.list(
    c("Parametric (Means/Variances assuming Normality)", 
      "Non-Parametric (Medians/Distributions without Normality assumption)"),
    multiple = FALSE,
    title = "Which distribution approach do you want to test?"
  )
  
  if (approach == "" || is.na(approach)) {
    cat("No approach selected. Exiting.\n")
    return(invisible(NULL))
  }
  
  cat(sprintf("\n[Reminder] Selected Approach: %s\n", approach))
  
  # Paired vs Independent
  dependency <- select.list(
    c("Independent (Groups are statistically independent)", 
      "Dependent / Paired (Variables are matched/measured on same subjects)"),
    multiple = FALSE,
    title = "Are the selected variables Dependent or Independent?"
  )
  
  if (dependency == "" || is.na(dependency)) {
     cat("Dependency type not selected. Exiting.\n")
     return(invisible(NULL))
  }
  
  is_paired <- grepl("Dependent", dependency)
  
  cat("\n--------------------------------------------------\n")
  cat(" RUNNING HOMOGENEITY & DISTRIBUTION TESTS\n")
  cat("--------------------------------------------------\n")
  
  if (grepl("Parametric", approach)) {
    if (k == 2) {
      # Setup complete cases if paired, otherwise use available data respectively
      if (is_paired) {
        comp <- complete.cases(df[, selected_vars])
        v1 <- df[comp, selected_vars[1]]
        v2 <- df[comp, selected_vars[2]]
      } else {
        v1 <- as.numeric(na.omit(df[[selected_vars[1]]]))
        v2 <- as.numeric(na.omit(df[[selected_vars[2]]]))
      }
      
      cat(sprintf("\n=> Two-Sample %s T-Test\n", if(is_paired) "Paired" else "Independent"))
      tres <- tryCatch(t.test(v1, v2, paired = is_paired, conf.level = 1 - alpha), error = function(e) e)
      
      if (!inherits(tres, "error")) {
        cat(sprintf("   T-statistic: %.4f,  df: %.4f\n", tres$statistic, tres$parameter))
        cat(sprintf("   P-value    : %.4g\n", tres$p.value))
        if (tres$p.value < alpha) {
          cat("   Conclusion : Reject Null Hypothesis (Means are significantly different)\n")
        } else {
          cat("   Conclusion : Failed to reject Null Hypothesis (No significant difference in means)\n")
        }
      } else {
        cat("   Error running T-test:", conditionMessage(tres), "\n")
      }
      
      if (!is_paired) {
        cat("\n=> F-Test for Homogeneity of Variances\n")
        fres <- tryCatch(var.test(v1, v2, conf.level = 1 - alpha), error = function(e) e)
        if (!inherits(fres, "error")) {
          cat(sprintf("   F-statistic: %.4f,  df1: %d, df2: %d\n", fres$statistic, fres$parameter[1], fres$parameter[2]))
          cat(sprintf("   P-value    : %.4g\n", fres$p.value))
          if (fres$p.value < alpha) {
            cat("   Conclusion : Reject Null (Variances are significantly different / Non-Homogeneous)\n")
          } else {
            cat("   Conclusion : Failed to reject Null (Variances are effectively Homogeneous)\n")
          }
        }
      }
      
    } else {
      # k > 2 Parametric
      cat(sprintf("\n=> One-Way %s ANOVA\n", if(is_paired) "Repeated Measures" else "Independent"))
      
      data_list <- lapply(selected_vars, function(v) df[[v]])
      long_df <- data.frame(
        value = unlist(data_list),
        group = factor(rep(selected_vars, each = nrow(df))),
        id = factor(rep(1:nrow(df), times = k))
      )
      
      if (is_paired) {
        aov_res <- tryCatch(suppressWarnings(aov(value ~ group + Error(id/group), data = long_df)), error = function(e) e)
        if (!inherits(aov_res, "error")) {
           print(summary(aov_res))
           cat("\n   Conclusion: Check the 'Pr(>F)' column above. If < alpha, means are significantly different.\n")
        } else {
           cat("   Error running Repeated Measures ANOVA:", conditionMessage(aov_res), "\n")
        }
      } else {
        aov_res <- tryCatch(aov(value ~ group, data = long_df), error = function(e) e)
        if (!inherits(aov_res, "error")) {
           sm <- summary(aov_res)
           print(sm)
           pval <- sm[[1]][["Pr(>F)"]][1]
           
           if (!is.null(pval) && !is.na(pval)) {
             if (pval < alpha) {
               cat("\n   Conclusion : Reject Null (Means are significantly different across at least one pair of variables)\n")
             } else {
               cat("\n   Conclusion : Failed to reject Null (No significant difference in means)\n")
             }
           }
        }
        
        cat("\n=> Bartlett Test for Homogeneity of Variances\n")
        bart_res <- tryCatch(bartlett.test(value ~ group, data = long_df), error = function(e) e)
        if (!inherits(bart_res, "error")) {
           cat(sprintf("   K-squared  : %.4f,  df: %d\n", bart_res$statistic, bart_res$parameter))
           cat(sprintf("   P-value    : %.4g\n", bart_res$p.value))
           if (bart_res$p.value < alpha) {
             cat("   Conclusion : Reject Null (Variances are significantly different / Non-Homogeneous)\n")
           } else {
             cat("   Conclusion : Failed to reject Null (Variances are Homogeneous)\n")
           }
        }
      }
    }
  } else {
    # Non-Parametric Framework
    if (k == 2) {
      if (is_paired) {
        comp <- complete.cases(df[, selected_vars])
        v1 <- df[comp, selected_vars[1]]
        v2 <- df[comp, selected_vars[2]]
        
        cat("\n=> Wilcoxon Signed-Rank Test (Testing paired median shifts)\n")
        wxres <- tryCatch(suppressWarnings(wilcox.test(v1, v2, paired = TRUE, conf.level = 1 - alpha)), error = function(e) e)
        if (!inherits(wxres, "error")) {
           cat(sprintf("   V-statistic: %.4f\n", wxres$statistic))
           cat(sprintf("   P-value    : %.4g\n", wxres$p.value))
           if (wxres$p.value < alpha) {
             cat("   Conclusion : Reject Null (Paired medians are significantly different)\n")
           } else {
             cat("   Conclusion : Failed to reject Null (No significant difference in paired medians)\n")
           }
        }
      } else {
        v1 <- as.numeric(na.omit(df[[selected_vars[1]]]))
        v2 <- as.numeric(na.omit(df[[selected_vars[2]]]))
        
        cat("\n=> Two-Sample Kolmogorov-Smirnov Test (Homogeneity of overall distributions)\n")
        ksres <- tryCatch(suppressWarnings(ks.test(v1, v2)), error = function(e) e)
        if (!inherits(ksres, "error")) {
           cat(sprintf("   D-statistic: %.4f\n", ksres$statistic))
           cat(sprintf("   P-value    : %.4g\n", ksres$p.value))
           if (ksres$p.value < alpha) {
             cat("   Conclusion : Reject Null (The two variables do not come from the same distribution)\n")
           } else {
             cat("   Conclusion : Failed to reject Null (Distributions are fundamentally homogeneous)\n")
           }
        }
        
        cat("\n=> Wilcoxon Rank Sum Test (Testing shift in medians)\n")
        wxres <- tryCatch(suppressWarnings(wilcox.test(v1, v2, paired = FALSE, conf.level = 1 - alpha)), error = function(e) e)
        if (!inherits(wxres, "error")) {
           cat(sprintf("   W-statistic: %.4f\n", wxres$statistic))
           cat(sprintf("   P-value    : %.4g\n", wxres$p.value))
           if (wxres$p.value < alpha) {
             cat("   Conclusion : Reject Null (Medians are significantly different)\n")
           } else {
             cat("   Conclusion : Failed to reject Null (No significant difference in medians)\n")
           }
        }
      }
    } else {
      # k > 2 Non-Parametric
      if (is_paired) {
        cat("\n=> Friedman Rank Sum Test (Testing multiple paired/dependent samples)\n")
        mat <- as.matrix(df[, selected_vars])
        mat <- mat[complete.cases(mat), ]
        
        if (nrow(mat) < 2) {
          cat("   Error: Not enough complete valid observations across all selected variables.\n")
        } else {
          fres <- tryCatch(friedman.test(mat), error = function(e) e)
          if (!inherits(fres, "error")) {
             cat(sprintf("   Friedman chi-squared: %.4f,  df: %d\n", fres$statistic, fres$parameter))
             cat(sprintf("   P-value    : %.4g\n", fres$p.value))
             if (fres$p.value < alpha) {
                cat("   Conclusion : Reject Null (Significant distributional differences among the paired variables)\n")
             } else {
                cat("   Conclusion : Failed to reject Null (Variables practically follow the same distribution)\n")
             }
          }
        }
      } else {
        cat("\n=> Kruskal-Wallis Rank Sum Test (Testing multiple independent samples)\n")
        data_list <- lapply(selected_vars, function(v) df[[v]])
        long_df <- data.frame(
          value = unlist(data_list),
          group = factor(rep(selected_vars, each = nrow(df)))
        )
        
        kwres <- tryCatch(kruskal.test(value ~ group, data = long_df), error = function(e) e)
        if (!inherits(kwres, "error")) {
           cat(sprintf("   K-W chi-squared: %.4f,  df: %d\n", kwres$statistic, kwres$parameter))
           cat(sprintf("   P-value        : %.4g\n", kwres$p.value))
           if (kwres$p.value < alpha) {
              cat("   Conclusion : Reject Null (Significant differences found; not from same distribution)\n")
           } else {
              cat("   Conclusion : Failed to reject Null (Variables are homogeneous and likely from the same distribution)\n")
           }
        }
      }
    }
  }
  
  cat("\n==================================================\n")
  cat(" Testing Series Completed.\n")
  cat("==================================================\n")
}
