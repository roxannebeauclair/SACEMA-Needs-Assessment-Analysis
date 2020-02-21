# Author: Roxanne Beauclair

# Description: Importing the CSV file downloaded from Checkbox
# and saving as an R file.

# ===================
# File paths
# ===================

wd <- getwd()
rdata <- paste0(wd, "/Data/Raw") 
cdata <- paste0(wd, "/Data/Cleaned")
data <- paste0(rdata, "/Needs_Assessment_21.02.2020.csv")

importdf <- paste0(cdata, "/01-imported.rda")
fxn <- paste0(wd, "/Scripts/00-Functions.R")

# ====================
# Loading dependencies
# ====================
source(fxn)

# tidyverse: for data management functions


InstallLoad("tidyverse")

# ===========
# Import data
# ===========

df <- read_csv(file = data)


# ==============
# Save dataset
# ==============

save(df, file = importdf)

# ================
# Remove libraries 
# ================

Vectorize(detach)(name = paste0("package:", c("tidyverse")), 
                  unload = TRUE, 
                  character.only = TRUE)

rm(list=ls())