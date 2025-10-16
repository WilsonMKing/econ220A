################################################################################
################################################################################

### Problem Set 2
### Name: Wilson King

### Preamble

# Load Packages
packages <- c("haven", "dplyr", "tidyr", "magrittr", "ggplot2", "stargazer", "readr", "rshape2")
lapply(packages, require, character.only = TRUE)

# Set file paths
root_path <- paste0("/Users/", Sys.getenv("USER"), "/Documents/GitHub/econ220A/")
root_code <- paste0(root_path, "code/")
root_data <- paste0(root_path, "data/")
root_exhibits <- paste0(root_path, "exhibits/pset2/")

### Problem 1

# Function: Compute Market Shares
compute_market_shares <- function(
    delta, # mean utility (linear)
    sigma, # coefficient 
    X_s, # sugar content per gram
    Xbar_s, # average sugar content per gram in city 1 in period 1
    N = 50){ # number of consumers
  
  # Set grid of u_i
  u_i <- seq(0.1, 0.9, length.out = 50)
  
  # Create v_i
  v_i <- qnorm(u_i)
  
  # Compute mean utility
  mean_u <- delta + sigma*v_i*(X_s - Xbar_s)
  
  # Compute numerator of market share
  num <- exp(mean_u)
  
  # Compute denominator of market share
  denom <- 1 + sum(num)
  
  # Compute vector of market shares
  s_ijct <- num / denom
  
  # Compute aggregate market share
  sjct <- sum(s_ijct) / N
  
  return(sjct)
  
}




