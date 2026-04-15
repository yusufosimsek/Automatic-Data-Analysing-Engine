##----------------------------------------------------------------------------------------------------##
## Missing Value Imputation Engine
##----------------------------------------------------------------------------------------------------##

Run_Imputation <- function(df) {
  if (!is.data.frame(df)) {
    stop("Error: Please supply a valid dataframe object.")
  }
  
  cat("\n==================================================\n")
  cat(" Missing Value (NA) Imputation Engine\n")
  cat("==================================================\n")
  
  repeat {
    # 1. Dynamically scan all columns for NA footprints
    bad_cols <- sapply(df, function(x) sum(is.na(x)))
    missing_vars <- names(bad_cols[bad_cols > 0])
    
    if (length(missing_vars) == 0) {
      cat("\n=> System Check Clean: There are 0 missing (NA) values remaining in the explicit dataframe!\n")
      break
    }
    
    cat("\n[Network Warning]: Variables currently suffering from missing logic gaps:\n")
    for (v in missing_vars) {
      cat(sprintf("  - %s : %d missing record(s)\n", v, bad_cols[v]))
    }
    
    cat("\n")
    target_var <- select.list(
      c(missing_vars, "Exit Imputation Engine"),
      multiple = FALSE,
      title = "Which mapped variable do you want to repair/impute right now?"
    )
    
    if (target_var == "" || grepl("Exit", target_var) || is.na(target_var)) {
      cat("\n=> Extracting out of Imputation matrix.\n")
      break
    }
    
    is_num <- is.numeric(df[[target_var]])
    
    # Render different menu options based on underlying mathematical mapping
    if (is_num) {
       method <- select.list(
         c("1. Replace gap with arithmetic Mean (Parametric mapping)", 
           "2. Replace gap with internal Median (Robust against heavy outliers)", 
           "3. Replace gap with structural Mode (Most frequently occurring)",
           "4. Manually override gap with a Custom Value",
           "5. Drop/Delete the entire row containing the NA"),
         multiple = FALSE,
         title = sprintf("Select algorithm to repair numeric variable '%s':", target_var)
       )
    } else {
       method <- select.list(
         c("1. Replace gap with structural Mode (Most frequently occurring category)",
           "2. Manually override gap with a Custom Text/Factor Value",
           "3. Drop/Delete the entire row containing the NA"),
         multiple = FALSE,
         title = sprintf("Select algorithm to repair categorical variable '%s':", target_var)
       )
    }
    
    if (method == "" || is.na(method)) next
    
    missing_idx <- is.na(df[[target_var]])
    num_missing <- sum(missing_idx)
    var_clean <- na.omit(df[[target_var]])
    
    cat("\n--------------------------------------------------\n")
    
    # ==============================
    # MEAN
    # ==============================
    if (grepl("Mean", method)) {
       imp_val <- mean(var_clean)
       df[[target_var]][missing_idx] <- imp_val
       cat(sprintf("=> Successfully replaced %d gap(s) with the computed Mean: %.4f\n", num_missing, imp_val))
       
    # ==============================
    # MEDIAN
    # ==============================
    } else if (grepl("Median", method)) {
       imp_val <- median(var_clean)
       df[[target_var]][missing_idx] <- imp_val
       cat(sprintf("=> Successfully replaced %d gap(s) with the median shift: %.4f\n", num_missing, imp_val))
       
    # ==============================
    # MODE
    # ==============================
    } else if (grepl("Mode", method)) {
       val_table <- table(var_clean)
       imp_val <- names(val_table)[which.max(val_table)]
       
       # Respect intrinsic bounds
       if (is_num) {
          imp_val <- as.numeric(imp_val)
       } else if (is.character(df[[target_var]])) {
          imp_val <- as.character(imp_val)
       } else if (is.factor(df[[target_var]])) {
          # Make sure factor has the level before injecting
          levels(df[[target_var]]) <- unique(c(levels(df[[target_var]]), imp_val))
       }
       
       df[[target_var]][missing_idx] <- imp_val
       cat(sprintf("=> Successfully replaced %d gap(s) with dominant Mode: %s\n", num_missing, as.character(imp_val)))
       
    # ==============================
    # CUSTOM OVERRIDE
    # ==============================
    } else if (grepl("Custom Value", method)) {
       custom_val <- readline(prompt=sprintf("Type the exact implant character/number to patch into '%s': ", target_var))
       
       if (is_num) {
         imp_val <- suppressWarnings(as.numeric(custom_val))
         if(is.na(imp_val)) {
            cat("\nError: Mapped variable explicitly requires numeric logic, but string characters were input. Fix aborted.\n")
            next
         }
       } else {
         imp_val <- custom_val
         if (is.factor(df[[target_var]])) {
            levels(df[[target_var]]) <- unique(c(levels(df[[target_var]]), imp_val))
         }
       }
       
       df[[target_var]][missing_idx] <- imp_val
       cat(sprintf("=> Custom Override Success: Replaced %d gap(s) directly with: %s\n", num_missing, as.character(imp_val)))
       
    # ==============================
    # ROW DROP DELETION
    # ==============================
    } else if (grepl("Drop", method)) {
       # Delete entire rows sequentially
       df <- df[!is.na(df[[target_var]]), ]
       cat(sprintf("=> Total Deletion: Deleted %d explicit rows containing broken data arrays.\n", num_missing))
    }
  }
  
  cat("\n==================================================\n")
  cat(" Note: Code block is returning explicitly repaired Dataframe.\n")
  cat(" Run using: df <- Run_Imputation(df)\n")
  cat("==================================================\n\n")
  
  return(df)
}
