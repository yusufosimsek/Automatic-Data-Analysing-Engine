# Alpha_Beta_Determiner.R

get_alpha_beta <- function() {
  cat("\n--- Parameter Determination for Hypothesis Testing ---\n")
  
  # Get alpha value
  repeat {
    alpha_input <- readline(prompt="Please enter the significance level (alpha) (e.g., 0.05, 0.01, 0.10): ")
    
    alpha <- suppressWarnings(as.numeric(alpha_input))
    
    if (!is.na(alpha) && alpha > 0 && alpha < 1) {
      break
    } else {
      cat("Invalid input. Alpha must be a numeric value between 0 and 1.\n")
    }
  }
  
  # Get beta value
  repeat {
    beta_input <- readline(prompt="Please enter the probability of Type II error (beta) (e.g., 0.20, 0.10, 0.05): ")
    
    beta <- suppressWarnings(as.numeric(beta_input))
    
    if (!is.na(beta) && beta > 0 && beta < 1) {
      break
    } else {
      cat("Invalid input. Beta must be a numeric value between 0 and 1.\n")
    }
  }
  
  cat(sprintf("\n[Success] Values accepted: Alpha = %g, Beta = %g\n", alpha, beta))
  cat("These values can now be used for subsequent hypothesis and dependency/homogeneity tests.\n")
  
  # Return the values as a list so they can be used by other parts of the program
  return(list(alpha = alpha, beta = beta))
}

# Execution block (runs when the script is sourced or run interactively)
if (sys.nframe() == 0 || interactive()) {
  # Call the function and store the result
  test_parameters <- get_alpha_beta()
  
  # Assign to the global environment to make them globally accessible
  # by other scripts that will run after this one.
  assign("global_alpha", test_parameters$alpha, envir = .GlobalEnv)
  assign("global_beta", test_parameters$beta, envir = .GlobalEnv)
}
