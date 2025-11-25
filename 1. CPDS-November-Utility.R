#Load necessary packages
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(writexl)
library(ggplot2)
library(glue)

setwd("C:/Users/U457174/SCOTS Connect/Agricultural Statistics Unit - Economic Statistics/TIFF/Cereals November 2025/Processing/Code outputs")

# Set crop year 
crop_year <- "2025-26"

#load in main cereal sample csv
Sample<-read_csv("2025-26 - Production - Materials - Sample - Main_sample_2025 - Excludes test farms.csv")

#load in raw cereal survey data tsv from "all cereal forms" export
df_raw <- read_tsv("QuickStatsExtract 680 All (10).tsv") 

#load in june census areas
june_census <-read_csv("june_census_areas_practice_itl225.csv") 

#Read in csv of manual removals in processing i.e FF, outliers etc
removals_FF <- read_csv("Removals.csv")

#Read in yield QA log (this will be empty until first lot of QA run)
Yield_QA_Log <- read_excel("2025-26 - November - Production - Data - QA - Production values flagged in QA as unexpected yield (manual bounds) - LOG.xlsx")

# Strings used when naming exports 
str1 <- "Cereal Production and Disposal Survey - "
str3 <- ".csv"
str7 <- ".xlsx"










  
  
  
  
  
  
  
  
  
  
  




