##----------------------------------------------------------------------------------------------------##
## Data Manipulation Machine (Data Correction & Type Casting)
##----------------------------------------------------------------------------------------------------##

Interactive_Data_Fixer <- function(df) {
  if (!is.data.frame(df)) {
    stop("Error: Please enter a valid dataframe to manipulate.")
  }
  
  cat("\n==================================================\n")
  cat(" Data Manipulation Machine\n")
  cat("==================================================\n")
  
  repeat {
    # 1. Main Action Menu
    action <- select.list(
      c("1. Replace/Fix a specific wrong value in a variable",
        "2. Change Data Type of a variable (Numeric/Factor/Character/etc.)",
        "3. Exit Manipulation Machine"),
      multiple = FALSE,
      title = "What manipulation would you like to perform right now?"
    )
    
    if (action == "" || grepl("Exit", action) || is.na(action)) {
      cat("\n=> Exiting the Data Manipulation Machine.\n")
      break
    }
    
    # -----------------------------------------------------------------
    # VALUE FIXING LOGIC
    # -----------------------------------------------------------------
    if (grepl("Replace/Fix", action)) {
      col_to_fix <- select.list(colnames(df), title="Select the variable/column to fix:")
      if (col_to_fix == "") next
      
      cats <- as.character(unique(df[[col_to_fix]]))
      
      if (length(cats) > 50) {
        cat(sprintf("\n[Notice]: Variable '%s' has a massive amount of unique values. Please type carefully.\n", col_to_fix))
      } else {
        cat(sprintf("\n=> Current distinct values found inside '%s':\n", col_to_fix))
        print(cats)
      }
      
      cat("\n")
      wrong_val <- readline(prompt="Type the EXACT value you want to trace and replace (or type 'NA' to replace missing cells): ")
      new_val <- readline(prompt="Type the NEW corrected value to implant: ")
      
      matches <- 0
      
      if (toupper(wrong_val) == "NA") {
        # NA Replacement Handling
        is_missing <- is.na(df[[col_to_fix]])
        matches <- sum(is_missing)
        if (matches > 0) {
          # Handle typing based on column logic
          if (is.numeric(df[[col_to_fix]])) {
             new_val_cast <- suppressWarnings(as.numeric(new_val))
             if(is.na(new_val_cast)) cat("\n[Warning] Replacing numeric NAs with a non-number will force variable into Character class!\n")
             df[[col_to_fix]][is_missing] <- new_val
          } else {
             df[[col_to_fix]][is_missing] <- new_val
          }
        }
      } else {
        # Standard Value matching String comparison mapping to avoid rigid strict typing errors
        char_col <- as.character(df[[col_to_fix]])
        idx <- which(char_col == wrong_val & !is.na(char_col))
        matches <- length(idx)
        
        if (matches > 0) {
          
          # If it's a strongly typed factor, we must append the new level physically first
          if (is.factor(df[[col_to_fix]])) {
             levels(df[[col_to_fix]]) <- c(levels(df[[col_to_fix]]), new_val)
             df[[col_to_fix]][idx] <- new_val
          } else {
             # Replace string/number
             df[[col_to_fix]][idx] <- new_val
             
             # If mapping strings into numeric arrays abruptly converts the entire array to char, we rely on Type Casting tool block to fix.
          }
        }
      }
      
      if (matches == 0) {
         cat(sprintf("\n=> No matches found for '%s'. Nothing fundamentally changed.\n", wrong_val))
      } else {
         cat(sprintf("\n=> [Success]: Physically replaced %d occurrence(s) of '%s' with '%s' inside variable '%s'.\n", matches, wrong_val, new_val, col_to_fix))
      }
      
    # -----------------------------------------------------------------
    # DATA TYPE CASTING LOGIC
    # -----------------------------------------------------------------
    } else if (grepl("Change Data Type", action)) {
      
      col_to_fix <- select.list(colnames(df), title="Select the variable to alter mathematical typing:")
      if (col_to_fix == "") next
      
      curr_type <- class(df[[col_to_fix]])[1]
      cat(sprintf("\n=> Current mathematical structure mapping of '%s' is strictly: [%s]\n", col_to_fix, toupper(curr_type)))
      
      new_type <- select.list(
        c("Numeric   (Convert strings to decimals layout)", 
          "Integer   (Convert to explicit whole numbers)", 
          "Character (Force treat as strict string text)", 
          "Factor    (Categorize for ANOVA/Chi-Square)", 
          "Logical   (Convert True/False layout)"),
        multiple = FALSE,
        title = "What explicit data type class do you want to force it to?"
      )
      
      if (new_type == "" || is.na(new_type)) next
      
      new_type_clean <- strsplit(new_type, " ")[[1]][1]
      
      old_nas <- sum(is.na(df[[col_to_fix]]))
      new_vec <- NULL
      
      cat(sprintf("\n=> Initiating conversion engine to construct variable into '%s' format...\n", new_type_clean))
      
      if (new_type_clean == "Numeric") {
        new_vec <- suppressWarnings(as.numeric(as.character(df[[col_to_fix]])))
      } else if (new_type_clean == "Integer") {
        new_vec <- suppressWarnings(as.integer(as.character(df[[col_to_fix]])))
      } else if (new_type_clean == "Character") {
        new_vec <- as.character(df[[col_to_fix]])
      } else if (new_type_clean == "Factor") {
        new_vec <- as.factor(df[[col_to_fix]])
      } else if (new_type_clean == "Logical") {
        new_vec <- suppressWarnings(as.logical(as.character(df[[col_to_fix]])))
      }
      
      # Safety logic - Check if mapping forced valid entries to break
      new_nas <- sum(is.na(new_vec))
      diff_nas <- new_nas - old_nas
      
      if (diff_nas > 0) {
        cat(sprintf("\n[WARNING]: Strict Conversion abruptly dissolved %d incompatible values into missing (NA) metrics!\n", diff_nas))
        cat("          If this was a mistake, do not save the exported dataframe back to your variable.\n")
      }
      
      df[[col_to_fix]] <- new_vec
      cat(sprintf("\n=> [Success]: Variable '%s' successfully reconstructed computationally as %s.\n", col_to_fix, class(df[[col_to_fix]])[1]))
    }
  }
  
  cat("\n==================================================\n")
  cat(" Note: Code block returning the fully cleaned Dataframe.\n")
  cat(" Make sure in your root script to assign this execution back to your variable frame!\n")
  cat(" Example: df <- Interactive_Data_Fixer(df)\n")
  cat("==================================================\n\n")
  
  return(df)
}
