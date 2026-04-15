##----------------------------------------------------------------------------------------------------##
## Master Analytics Engine
##----------------------------------------------------------------------------------------------------##

cat("=> Loading Essential Analytical Libraries...\n")
suppressPackageStartupMessages({
  if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
  if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
  if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
  if (!require(moments)) install.packages("moments"); library(moments)
})

cat("=> Securing Internal Network Framework Modules...\n")
modules <- c(
  "Alpha_Beta_Determiner.R",
  "Data_Manipulation_Machine.R",
  "Missing_Value_Imputation.R",
  "Outlier_Handling_Machine.R",
  "Data_Transformation_Packet.R",
  "Descriptive_Statistic_Packet.R",
  "Normality_Test_Packet.R",
  "Point_Estimation_and_CI.R",
  "Distribution_Comparison_Tests.R",
  "Correlation_Tests.R",
  "Categorical_ChiSquare_Tests.R",
  "Advanced_Model_Tests.R"
)

# Iteratively source all operational capabilities required dynamically at runtime.
for (mod in modules) {
  if (file.exists(mod)) {
    source(mod)
  } else {
    cat(sprintf("[Warning] Could not physically locate module block: %s in directory workspace.\n", mod))
  }
}

##----------------------------------------------------------------------------------------------------##
## PRIMARY EXECUTION WRAPPER
##----------------------------------------------------------------------------------------------------##
Start_Analysis_Framework <- function(df) {
  
  if (!is.data.frame(df)) {
    stop("Critical Integrity Fault: The Master Engine mandates an explicit Base Dataframe array to compile properly.")
  }
  
  # Setup Deep Caching for Safe Undo Rerouting
  current_df <- df
  backup_df <- df
  
  cat("\n=========================================================================\n")
  cat("           UNIVERSAL R-ANALYTICS MASTER AUTOMATION ENGINE\n")
  cat("=========================================================================\n")
  
  repeat {
    menu_options <- c(
      "1.  Configure Alpha & Beta Thresholds (Core Prerequisite)",
      "2.  Data Cleansing: Fix corrupted values or Override Matrix Types",
      "3.  Data Cleansing: Advanced Missing Value (NA) Imputation",
      "4.  Data Cleansing: Extreme Outlier Scanning & Mitigation Logic",
      "5.  Data Cleansing: Mathematical Column Transformations (Log/Sqrt/Norm/Z-Score)",
      "6.  Descriptive Statistics & Automated Visualization Packages",
      "7.  Distribution Phase: Normality Testing Suite (Shapiro-Wilk)",
      "8.  Hypothesis Phase I: Point Estimations & Specific Mathematical C.I.",
      "9.  Hypothesis Phase II: Homogeneity & Group Distribution Matching",
      "10. Hypothesis Phase III: Correlation Suite (Pearson/Spearman/Kendall)",
      "11. Hypothesis Phase IV: Categorical Matrices (Dual Chi-Square Systems)",
      "12. Advanced Modeling: ANOVA, MANOVA, Multi-Variate GLM & Non-Parametric arrays",
      "13. => !! UNDO LAST SYSTEM DATA OVERRIDE ROUTINE !!",
      "14. [ EXIT MASTER ENGINE ] - Save current Array Matrix state to terminal."
    )
    
    cat("\n-------------------------------------------------------------------------\n")
    selection <- select.list(
      menu_options,
      multiple = FALSE,
      title = "MAIN MENU DASHBOARD: Which distinct operation process do you want to route?"
    )
    
    if (selection == "" || grepl("EXIT", selection)) {
      cat("\n=> Safely powering down internal Master Engine...\n")
      cat("=> Extracting Final calculated Dataframe structures iteratively...\n")
      break
    }
    
    # ------------------
    # SAFELOCK PREREQUISITE ALGORITHM
    # ------------------
    check_prerequisites <- function(test_name) {
      if (!exists("global_alpha", envir = .GlobalEnv) || !exists("global_beta", envir = .GlobalEnv)) {
        cat(sprintf("\n[SYSTEM LOCK]: You logically cannot perform the [%s] module matrix right now.\n", test_name))
        cat("Reason: The explicitly required mathematical parameters (Alpha & Beta constraints) remain unassigned.\n")
        cat("Action: Please route 'Configure Alpha & Beta Thresholds' algorithm manually from the Menu first!\n")
        return(FALSE)
      }
      return(TRUE)
    }
    
    # ------------------
    # MENU ROUTING FIREWALL LOGIC
    # ------------------
    
    # Route 1: Alpha Beta Settings
    if (grepl("Configure Alpha", selection)) {
      if(exists("get_alpha_beta")) get_alpha_beta() else cat("[Block Note] Associated script missing.\n")
      
    # Route 2: Physical Fixer
    } else if (grepl("Fix corrupted values", selection)) {
      backup_df <- current_df # Clone array immediately saving active undo logic state
      if(exists("Interactive_Data_Fixer")) current_df <- Interactive_Data_Fixer(current_df) else cat("[Block Note] Associated script missing.\n")
      
    # Route 3: Imputation Engine
    } else if (grepl("Imputation", selection)) {
      backup_df <- current_df
      if(exists("Run_Imputation")) current_df <- Run_Imputation(current_df) else cat("[Block Note] Associated script missing.\n")
      
    # Route 4: Outlier Handlers
    } else if (grepl("Outlier Scanning", selection)) {
      backup_df <- current_df
      if(exists("Run_Outlier_Handling")) current_df <- Run_Outlier_Handling(current_df) else cat("[Block Note] Associated script missing.\n")
      
    # Route 5: Transformations
    } else if (grepl("Mathematical Column Trans", selection)) {
      backup_df <- current_df
      if(exists("Run_Data_Transformation")) current_df <- Run_Data_Transformation(current_df) else cat("[Block Note] Associated script missing.\n")
      
    # Route 6: Descriptive Stats
    } else if (grepl("Descriptive Stats", selection)) {
      if(exists("Analyze_Dataframe")) Analyze_Dataframe(current_df) else cat("[Block Note] Associated script missing.\n")
      
    # Route 7: Normality Setup
    } else if (grepl("Normality Testing", selection)) {
      if(exists("Test_Normality")) Test_Normality(current_df) else cat("[Block Note] Associated script missing.\n")
      
    # Route 8: Point Estimates (REQUIRES LOCK)
    } else if (grepl("Point Estimations", selection)) {
      if (check_prerequisites("Point Estimations Test Logic")) {
         if(exists("Estimate_Point_and_CI")) Estimate_Point_and_CI(current_df) else cat("[Block Note] Associated script missing.\n")
      }
      
    # Route 9: Distribution Homogeneity (REQUIRES LOCK)
    } else if (grepl("Homogeneity & Group", selection)) {
      if (check_prerequisites("Distribution / Homogeneity Suite")) {
         if(exists("Compare_Distributions")) Compare_Distributions(current_df) else cat("[Block Note] Associated script missing.\n")
      }
      
    # Route 10: Correlations (REQUIRES LOCK)
    } else if (grepl("Correlation Suite", selection)) {
      if (check_prerequisites("Correlation Analysis Test Array")) {
         if(exists("Run_Correlation_Tests")) Run_Correlation_Tests(current_df) else cat("[Block Note] Associated script missing.\n")
      }
      
    # Route 11: Categorical Matrix (REQUIRES LOCK)
    } else if (grepl("Categorical Matrices", selection)) {
      if (check_prerequisites("Categorical Chi-Square Testing Frame")) {
         if(exists("Run_Categorical_Tests")) Run_Categorical_Tests(current_df) else cat("[Block Note] Associated script missing.\n")
      }
      
    # Route 12: Advanced Modeling (REQUIRES LOCK)
    } else if (grepl("Advanced Modeling", selection)) {
      if (check_prerequisites("ANOVA/MANOVA Linear System Matrix")) {
         if(exists("Run_Advanced_Models")) Run_Advanced_Models(current_df) else cat("[Block Note] Associated script missing.\n")
      }
      
    # Route 13: DEEP UNDO ABILITY
    } else if (grepl("UNDO LAST", selection)) {
      cat("\n[!] EMERGENCY STATE TRIGGERED: Erasing dataframe trajectory and restoring the exact predecessor matrix...\n")
      current_df <- backup_df
      cat("=> Success Output: System structure completely rolled back and stabilized.\n")
    }
  }
  
  cat("\n=========================================================================\n")
  cat(" Engine successfully terminated operations. Handing off finalized Dataset.\n")
  cat(" Save this terminal prompt state explicitly: e.g., 'df <- Start_Analysis_Framework(df)'\n")
  cat("=========================================================================\n")
  return(current_df)
}
