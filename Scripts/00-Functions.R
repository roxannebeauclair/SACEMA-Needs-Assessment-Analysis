# =================
# Importing dataset
# =================

# Author: Roxanne Beauclair
# Description: This scripts creates all of the functions that will 
# in the Herstory Analysis

# ================================================
# Functions for loading and detaching dependencies
# ================================================
InstallLoad <- function(package1, ...) {
  # This function will load, and install if necessary, libraries needed for script
  
  # Convert arguments to vector
  pkgs <- c(package1, ...)
  
  # Start loop to determine if each package is installed
  for(p in pkgs) {
    
    # If package is installed locally, load
    if(p %in% rownames(installed.packages()))
      do.call("library", list(p))
    
    # If package is not installed locally, download, then load
    else {
      install.packages(p)
      do.call("library", list(p))
    }
  }
}



# ==========================================
# Functions for producing summary statistics
# In particular table format
# ==========================================

# This function must be used with the tidyverse and
# svryr packages

# NOTES ON THE PROGRAMMING FUNCTIONS USED BELOW
# enquo() looks at the argument to see what I typed, and returns that
# value as a quosure. A quosure is a special type of formula
# The "!!" unquotes the quosure so it can be evaluated
# quo_name() creates a string out of the quosure

# ===========================================
# Functions to create univariate summary table
# ===========================================

# This function returns a table for a single categorical variable
# The table contains the frequencies (n) and proportions (Prop)
# in each category

recap_uni_catvar <- function(df, catvar) {
  
  data <- df
  var <- enquo(catvar)
  
  tab <- data %>%
    count(!!var, .drop = FALSE) %>%
    mutate(Prop = (n / sum(n)) * 100) %>%
    data.frame(row.names = 1) %>%
    round(1) %>%
    as.matrix() 
    
  # as.matrix() turns object into matrix which allows you to 
  # append matrices and preserving rownames as is.
  
  return(tab)
}

# This function returns a table for a single numeric variable
# The table contains the medians (n) and IQR (Prop)
# The table has the column names "n" and "Prop" so that 
# it can easily be appended to univariate summaries of
# categorical variables

recap_uni_numvar <- function(df, numvar) {
  
  data <- df
  var <- enquo(numvar)
  
  tab <- data %>%
    summarise(Median = median(!!var, na.rm = T),
              Q25 = quantile(!!var, 0.25, na.rm = T),
              Q75 = quantile(!!var, 0.75, na.rm = T)) %>%
    unite(Prop, Q25, Q75, sep = " - ") %>%
    rename(n = Median) %>%
    as.matrix()
  
  # as.matrix() turns object into matrix which allows you to 
  # append matrices and preserving rownames as is.
  
  row.names(tab) <- "Med and IQR"
  
  return(tab)
}




GetT1Stat2 <- function(var, byvar, continuous_fn = describeMedian, prop_fn = describeFactors){
  # This function gets description stats for building the htmlTable
  # You use this function when you have a 'by' variable
  require(Gmisc)
  
  getDescriptionStatsBy(var, 
                        byvar, 
                        html = TRUE,
                        continuous_fn = continuous_fn,
                        prop_fn = prop_fn,
                        header_count = TRUE,
                        statistics = TRUE,
                        # hrzl_prop = TRUE, FOR ROW %'s
                        add_total_col = TRUE,
                        digits = 1)
}