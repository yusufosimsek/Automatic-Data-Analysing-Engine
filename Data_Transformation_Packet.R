##----------------------------------------------------------------------------------------------------##
## Mathematical Data Transformation Engine
##----------------------------------------------------------------------------------------------------##

Run_Data_Transformation <- function(df) {
  if (!is.data.frame(df)) {
    stop("Error: Please supply a valid dataframe object.")
  }
  
  cat("\n==================================================\n")
  cat(" Mathematical Data Transformation Engine\n")
  cat("==================================================\n")
  
  repeat {
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    if (length(num_cols) == 0) {
      cat("\n=> System Halt: No explicit numeric variables available for mathematical translation matrix.\n")
      break
    }
    
    target_var <- select.list(
      c(num_cols, "Exit Transformation Engine"),
      multiple = FALSE,
      title = "Which mapped numeric variable do you want to transform?"
    )
    
    if (target_var == "" || grepl("Exit", target_var) || is.na(target_var)) {
      cat("\n=> Navigating out of Mathematical Transformation logic.\n")
      break
    }
    
    cat(sprintf("\n=> Actively querying structural dimensions for: '%s'\n", target_var))
    
    # Assess numeric extremes safely bridging negative logic bounds/inf loops
    min_val <- min(df[[target_var]], na.rm = TRUE)
    
    method <- select.list(
      c("1. Logarithmic transform (ln) - Pulls back positive right-skew data",
        "2. Log10 transform (base 10) - Highly aggressive scale shift",
        "3. Square Root transform - Tempers moderate right-skew logic (safe for 0)",
        "4. Inverse transform (1/x) - Maximum right-skew correction penalty",
        "5. Square transform (x^2) - Corrects negative left-skew data scaling",
        "6. Min-Max Normalization - Constrains boundaries explicitly between [0 to 1]",
        "7. Z-Score Standardization - Centers array around Mean=0 physically"),
      multiple = FALSE,
      title = sprintf("Select strict Mathematical Form to enforce upon '%s':", target_var)
    )
    
    if (method == "" || is.na(method)) next
    
    cat("\n--------------------------------------------------\n")
    
    orig_vec <- as.numeric(df[[target_var]])
    new_vec <- NULL
    suffix <- ""
    
    # ==============================
    # 1. NATURAL LOGARITHM
    # ==============================
    if (grepl("Logarithmic", method)) {
      if (min_val <= 0) {
        cat("[Safety Protocol Triggered]: Variable contains Zeroes or exact Negatives.\n")
        cat(" - Natural Log generates fatal (-Inf/NaN) logic states here.\n")
        cat(" - Auto-Executing shifted log(x + 1 - min(x)) implicitly.\n")
        new_vec <- log(orig_vec + 1 - min_val)
        suffix <- "_log"
      } else {
        new_vec <- log(orig_vec)
        suffix <- "_log"
      }
      
    # ==============================
    # 2. LOG BASE 10
    # ==============================
    } else if (grepl("Log10", method)) {
      if (min_val <= 0) {
        cat("[Safety Protocol Triggered]: Variable contains exact zeroes or negative footprints.\n")
        cat(" - Auto-Executing explicitly shifted log10(x + 1 - min(x)) tracking.\n")
        new_vec <- log10(orig_vec + 1 - min_val)
        suffix <- "_log10"
      } else {
        new_vec <- log10(orig_vec)
        suffix <- "_log10"
      }
      
    # ==============================
    # 3. SQUARE ROOT
    # ==============================
    } else if (grepl("Square Root", method)) {
      if (min_val < 0) {
         cat("[Safety Protocol Triggered]: Data contains negative footprints mapped internally.\n")
         cat(" - Explicit mathematical Sqrt outputs NaN parameters when assessing non-complex numbers < 0.\n")
         cat(" - Auto-Executing shifted geometry: sqrt(x - min(x)).\n")
         new_vec <- sqrt(orig_vec - min_val)
         suffix <- "_sqrt"
      } else {
         new_vec <- sqrt(orig_vec)
         suffix <- "_sqrt"
      }
      
    # ==============================
    # 4. INVERSE FLIP
    # ==============================
    } else if (grepl("Inverse", method)) {
      if (any(orig_vec == 0, na.rm=TRUE)) {
         cat("[Safety Protocol Triggered]: Matrix actively maps strict zero bounds (0).\n")
         cat(" - Computating (1/0) functionally crashes framework rendering (Inf).\n")
         cat(" - Adding fractional padding: 1 / (x + 1e-6).\n")
         new_vec <- 1 / (orig_vec + 1e-6)
      } else {
         new_vec <- 1 / orig_vec
      }
      suffix <- "_inv"
      
    # ==============================
    # 5. QUADRATIC MULTIPLIER
    # ==============================
    } else if (grepl("Square transform", method)) {
      new_vec <- orig_vec^2
      suffix <- "_sq"
      
    # ==============================
    # 6. BOUNDARY NORMALIZATION
    # ==============================
    } else if (grepl("Min-Max", method)) {
      val_max <- max(orig_vec, na.rm=TRUE)
      
      if (val_max - min_val == 0) {
         cat("\n[Error Output]: Mathematical Min-Max span computes explicitly to 0. Matrix completely flat. Aborting.\n")
         next
      } else {
         new_vec <- (orig_vec - min_val) / (val_max - min_val)
         suffix <- "_norm"
      }
      
    # ==============================
    # 7. Z-SCORE
    # ==============================
    } else if (grepl("Z-Score", method)) {
      new_vec <- as.numeric(scale(orig_vec, center=TRUE, scale=TRUE))
      suffix <- "_zscore"
    }
    
    # ------------------
    # User Decision Overwrite Parameters
    # ------------------
    cat("\n")
    overwrite <- select.list(
      c("1. Override and entirely replace the existing native variable",
        sprintf("2. Generate a totally NEW variable preserving original (New Named Output: %s%s)", target_var, suffix)),
      multiple = FALSE,
      title = "How do you want to handle the finalized dimension vector?"
    )
    
    if (grepl("NEW variable", overwrite)) {
       new_col_name <- paste0(target_var, suffix)
       df[[new_col_name]] <- new_vec
       cat(sprintf("\n=> Success Output: Engineered distinct appended column geometry: '%s'\n", new_col_name))
    } else if (grepl("Override", overwrite)){
       df[[target_var]] <- new_vec
       cat(sprintf("\n=> Success Output: Structurally overwrote physical array bounds of target variable '%s'.\n", target_var))
    } else {
       cat("\n=> Action specifically aborted externally.\n")
    }
  }
  
  cat("\n==================================================\n")
  cat(" Note: Code block must return explicitly generated Dataframes.\n")
  cat(" Run utilizing: df <- Run_Data_Transformation(df)\n")
  cat("==================================================\n\n")
  
  return(df)
}
