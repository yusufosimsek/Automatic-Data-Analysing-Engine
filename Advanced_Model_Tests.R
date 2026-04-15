##----------------------------------------------------------------------------------------------------##
## Advanced Linear Models & Non-Parametric Alternatives Package
##----------------------------------------------------------------------------------------------------##

Run_Advanced_Models <- function(df) {
  if (!is.data.frame(df)) {
    stop("Please enter a dataframe")
  }
  
  # 1. Remind Alpha & Beta from Global Env
  alpha <- if (exists("global_alpha")) get("global_alpha", envir = .GlobalEnv) else 0.05
  beta <- if (exists("global_beta")) get("global_beta", envir = .GlobalEnv) else 0.20
  
  cat("\n==================================================\n")
  cat(" Advanced Models (ANOVA, MANOVA, GLM, Non-Parametric)\n")
  cat("==================================================\n")
  cat(sprintf("[Reminder] Current Alpha: %g | Current Beta: %g\n", alpha, beta))
  
  # 2. Select Variables (Separated dynamically by Dependent vs Independent)
  all_cols <- colnames(df)
  
  dep_vars <- select.list(
    all_cols, 
    multiple = TRUE, 
    title = "Select DEPENDENT variable(s) [Target / Y]:"
  )
  
  if (length(dep_vars) == 0 || dep_vars[1] == "") {
    cat("\nNo dependent variables selected. Exiting.\n")
    return(invisible(NULL))
  }
  
  indep_vars <- select.list(
    all_cols, 
    multiple = TRUE, 
    title = "Select INDEPENDENT variable(s) [Predictors / X]:"
  )
  
  if (length(indep_vars) == 0 || indep_vars[1] == "") {
    cat("\nNo independent variables selected. Exiting.\n")
    return(invisible(NULL))
  }
  
  # 3. Remind Variable Types
  cat("\n[Reminder] Selected Variables & Types:\n")
  cat("  -> Dependent (Y):\n")
  for(v in dep_vars) {
    cat(sprintf("     - %s : %s\n", v, class(df[[v]])[1]))
  }
  cat("  -> Independent (X):\n")
  for(v in indep_vars) {
    cat(sprintf("     - %s : %s\n", v, class(df[[v]])[1]))
  }
  
  # 4. Remind Distribution Approaching 
  cat("\n[Reminder] Distribution Characteristics Setup for Linear Models:\n")
  cat("  -> Parametric ANOVA/MANOVA/Linear Models assume Normality of Residuals.\n")
  cat("  -> GLMs (Binomial/Poisson) use targeted non-normal distributions via distinct link functions.\n")
  cat("  -> Non-Parametric Equivalents bypass normality shapes entirely.\n\n")
  
  # 5. Which Modeling Framework
  test_choice <- select.list(
    c("ANOVA / ANCOVA (Parametric - 1 DV, 1+ IVs)", 
      "MANOVA (Parametric - Multiple DVs, 1+ IVs)", 
      "GLM (Generalized Linear Model - Linear/Logistic/Poisson)",
      "Non-Parametric Alternatives (Kruskal-Wallis / Rank-transformed regression)"),
    multiple = FALSE,
    title = "Which modeling approach do you want to test?"
  )
  
  if (test_choice == "" || is.na(test_choice)) {
    cat("No test selected. Exiting.\n")
    return(invisible(NULL))
  }
  
  cat("\n--------------------------------------------------\n")
  cat(sprintf(" RUNNING: %s\n", toupper(strsplit(test_choice, " ")[[1]][1])))
  cat("--------------------------------------------------\n")
  
  # Construct string formula logically
  dv_formula <- if(length(dep_vars) > 1) paste0("cbind(", paste(dep_vars, collapse=","), ")") else dep_vars[1]
  iv_formula <- paste(indep_vars, collapse=" + ")
  f_string <- paste(dv_formula, "~", iv_formula)
  form <- as.formula(f_string)
  
  # Clean NA data exclusively across chosen vars to maintain exact identical length for linear assumptions
  model_df <- df[, c(dep_vars, indep_vars), drop=FALSE]
  model_df <- model_df[complete.cases(model_df), , drop=FALSE]
  
  if (nrow(model_df) < 5) {
     cat("Error: Not enough complete valid observations to build predictive distributions.\n")
     return(invisible(NULL))
  }
  
  cat(sprintf("\n=> Prepared Formula: %s\n", f_string))
  cat(sprintf("=> Total complete observations optimized: %d\n\n", nrow(model_df)))
  
  # ===============================================
  # ANOVA logic
  # ===============================================
  if (grepl("ANOVA", test_choice) && !grepl("MANOVA|Non-Parametric", test_choice)) {
     if (length(dep_vars) > 1) {
       cat("Warning: Standard ANOVA implies 1 Dependent Variable. Prioritizing exactly 1.\n")
       form <- as.formula(paste(dep_vars[1], "~", iv_formula))
     }
     
     res <- tryCatch(aov(form, data=model_df), error=function(e) e)
     if (!inherits(res, "error")) {
       print(summary(res))
       
       # P-vals check explicitly mapping against alpha
       pvals <- summary(res)[[1]][["Pr(>F)"]]
       pvals <- pvals[!is.na(pvals)]
       if(any(pvals < alpha)) {
         cat(sprintf("\n   Conclusion: Reject Null (Statistically significant predictor effects detected at alpha=%g)\n", alpha))
       } else {
         cat(sprintf("\n   Conclusion: Failed to reject Null (No effects breached your alpha=%g boundary)\n", alpha))
       }
     } else {
       cat("Error running standard ANOVA:", conditionMessage(res), "\n")
     }
     
  # ===============================================
  # MANOVA logic
  # ===============================================
  } else if (grepl("MANOVA", test_choice)) {
     if (length(dep_vars) < 2) {
       cat("Error: MANOVA rigorously requires at least 2 Dependent Variables by architectural design. Exiting.\n")
     } else {
       res <- tryCatch(manova(form, data=model_df), error=function(e) e)
       if (!inherits(res, "error")) {
         sm <- summary(res, test="Pillai")
         print(sm)
         
         pvals <- sm$stats[,"Pr(>F)"]
         pvals <- pvals[!is.na(pvals)]
         if(any(pvals < alpha)) {
           cat(sprintf("\n   Conclusion: Reject Null (Statistically significant multivariable array tracking at alpha=%g)\n", alpha))
         } else {
           cat("\n   Conclusion: Failed to reject Null (No statistically significant impact tracking)\n")
         }
       } else {
         cat("Error constructing MANOVA:", conditionMessage(res), "\n")
       }
     }
     
  # ===============================================
  # GLM Logic
  # ===============================================
  } else if (grepl("GLM", test_choice)) {
     fam_choice <- select.list(
       c("gaussian (Standard Linear Regression)", 
         "binomial (Logistic Regression - Strict Binary DV)", 
         "poisson (Log-linear - Ordinal/Count DV)"),
       title = "Select GLM Function Class:"
     )
     
     fam <- if(fam_choice == "") "gaussian" else strsplit(fam_choice, " ")[[1]][1]
     
     if (length(dep_vars) > 1) {
       cat(sprintf("Warning: Base GLM tracks exactly 1 Dependent Variable per execution instance. Utilizing %s.\n", dep_vars[1]))
       form <- as.formula(paste(dep_vars[1], "~", iv_formula))
     }
     
     res <- tryCatch(glm(form, family=fam, data=model_df), error=function(e) e)
     if (!inherits(res, "error")) {
       print(summary(res))
       
       cat(sprintf("\n=> Null Deviance: %.2f | Residual Deviance: %.2f\n", res$null.deviance, res$deviance))
       
       # Extract matrix
       pvals <- summary(res)$coef[,4]
       sig_vars <- names(pvals)[pvals < alpha][-1] # Drops intercept natively
       if(length(sig_vars) > 0) {
         cat(sprintf("\n   Conclusion: Statistically significant predictors explicitly mapping to %g found: %s\n", alpha, paste(sig_vars, collapse=", ")))
       } else {
         cat(sprintf("\n   Conclusion: No functionally significant predictor impacts found at boundary=%g.\n", alpha))
       }
     } else {
       cat("Error configuring GLM bounds:", conditionMessage(res), "\n")
     }
     
  # ===============================================
  # NON-PARAMETRIC logic
  # ===============================================
  } else if (grepl("Non-Parametric", test_choice)) {
     if (length(dep_vars) > 1) {
       cat("Notice: Non-Parametric standard testing focuses on exactly 1 Target DV natively. Adjusting.\n")
     }
     dv <- dep_vars[1]
     
     if (length(indep_vars) == 1 && length(unique(model_df[[indep_vars[1]]])) > 2) {
       cat(sprintf("=> Running explicit Kruskal-Wallis Rank Sum for targeting %s via %s\n", dv, indep_vars[1]))
       form <- as.formula(paste(dv, "~", indep_vars[1]))
       res <- tryCatch(kruskal.test(form, data=model_df), error=function(e) e)
       if (!inherits(res, "error")) {
         cat(sprintf("   Chi-squared: %.4f,  Calculated df: %d\n", res$statistic, res$parameter))
         cat(sprintf("   P-value bounds  : %.4g\n", res$p.value))
         if (res$p.value < alpha) {
           cat("\n   Conclusion: Reject Null Hypothesis (Explicit distributions fundamentally fail alignment)\n")
         } else {
           cat("\n   Conclusion: Failed to reject Null Hypothesis (Distribution properties match mapping metrics)\n")
         }
       } else {
         cat("Error firing robust Kruskal-Wallis sequence:", conditionMessage(res), "\n")
       }
     } else {
       # Fallback: Ordinal Rank mapped Regression proxy for high dimensionality Non Parametrics
       cat("=> Highly dimensional Non-Parametric boundaries detected.\n")
       cat("=> Utilizing mapped Rank-transformed OLS calculation proxy logic...\n")
       
       model_df[[dv]] <- rank(model_df[[dv]])
       form <- as.formula(paste(dv, "~", iv_formula))
       res <- tryCatch(lm(form, data=model_df), error=function(e) e)
       if (!inherits(res, "error")) {
         print(summary(res))
         cat("\n   [Mathematical Note]: Data processed utilizing Ordinary Least Squares regression strictly against Ordinal specific Ranking boundaries.\n")
         pvals <- summary(res)$coef[,4]
         sig_vars <- names(pvals)[pvals < alpha][-1] 
         if(length(sig_vars) > 0) {
           cat(sprintf("   Conclusion: Statistically explicit mapped significance paths found at boundary=%g: %s\n", alpha, paste(sig_vars, collapse=", ")))
         } else {
           cat(sprintf("   Conclusion: Flat boundary - No significant predictors at %g threshold.\n", alpha))
         }
       } else {
         cat("Error operating robust variable translation bounds:", conditionMessage(res), "\n")
       }
     }
  }
  
  cat("\n==================================================\n")
  cat(" Modeling Processing Block Completed.\n")
  cat("==================================================\n")
}
