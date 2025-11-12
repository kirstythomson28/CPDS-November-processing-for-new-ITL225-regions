#Load necessary packages
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(writexl)
library(ggplot2)
library(glue)

# Set crop year 
crop_year <- "2025-26"

#load in main cereal sample csv
Sample<-read_csv("Cereal Production and Disposal Survey - 2025-26 - Production - Materials - Sample - Main_sample_2025 - Excludes test farms.csv")

#load in raw cereal survey data tsv from "all cereal forms" export
df_raw <- read_tsv("QuickStatsExtract 680 All (5).tsv") 

#Read in csv of manual removals in processing i.e FF, outliers etc
removals_FF <- read_csv("Removals.csv")

# Strings used when naming exports 
str1 <- "Cereal Production and Disposal Survey - "
str3 <- ".csv"
str7 <- ".xlsx"










  
  
  
  
  
  
  
  
  
  
  




