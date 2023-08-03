# exclude-safe-urls.R

# Take a CSV file with web URLs and exclude known safe/repetitive ones

setwd("c:/b/")

# LOAD LIBRARIES ----

options(repos="https://cran.ma.imperial.ac.uk/")

# Install the pacman package to call all the other packages
if (!require("pacman")) install.packages("pacman")

# Use pacman to install (if req) and load required packages
pacman::p_load(
  tidyverse
)

# import csv into a dataframe
df <- read.csv(file = "urls.csv")

# filter to exclude certain strings
df_filter <- df %>%
  dplyr::filter(!grepl('microsoft|nhs|windows|office.net|office.com|live.com|outlook.com|skype.com|doubleclick.net', RemoteUrl))

# export the filtered dataframe to a csv
write.csv(df_filter, file = "urls_processed.csv", row.names = FALSE)
