#Load necessary packages
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(writexl)
library(ggplot2)
library(glue)
library(tidyr)

# Define the folders you want
folders <- c("Setup documents", "Mail merge","November results", "QA files", "Publication charts")

# Create them if they don't exist
for (f in folders) {
  if (!dir.exists(f)) {
    dir.create(f)
  }
}

# Set crop year 
crop_year <- "2025-26"
CurrentYear = 2025

# load in main cereal sample csv
Sample <- read_csv(file.path("Setup documents",
                             "Main_sample_2025 - Excludes test farms.csv"))

# load in raw cereal survey data tsv from "all cereal forms" export
df_raw <- read_tsv(file.path("Setup documents", "QuickStatsExtract 680 All (10).tsv"))

# Load in November sample for mailmerge ex FF
full_data <- read_excel(file.path("Setup documents",
                                  "November sample for mailmerge ex FF.xlsx"))

# Load in manual removal records for mailmerge
manual_removals <- read_excel(file.path("Setup documents",
                                        "Manual removal records for mailmerge.xlsx"))

# load in june census areas
june_census <- read_csv(file.path("Setup documents", "june_census_areas_practice_itl225.csv"))

# Read in csv of manual removals in processing i.e FF, outliers etc
removals_FF <- read_csv(file.path("Setup documents", "Removals.csv"))

# Read in yield QA log (this will be empty until first lot of QA run)
Yield_QA_Log <- read_excel(file.path("Setup documents",
                                     "2025-26 - November - Production - Data - QA - LOG.xlsx"))

# Read in timeseries for charts 
ch_data <- read_csv(file.path("Setup documents", "CH_data_final.csv")) %>% 
  filter(Year > 0) %>%
  mutate(across(ends_with("_Production"), as.numeric),
         across(ends_with("_Area"), as.numeric),
         across(ends_with("_Yield"), as.numeric))

str(ch_data)


# Strings used when naming exports 
str1 <- "Cereal Production and Disposal Survey - "
str3 <- ".csv"
str7 <- ".xlsx"











  
  
  
  
  
  
  
  
  
  
  




