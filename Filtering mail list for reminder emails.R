setwd("~/Cereal survey/Cereal survey 2025-26/November")
library(readr)
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)

#Read in data set of those who have submitted survey and the original full data set 
submitted_data <- read_tsv("QuickStatsExtract 680 All (10).tsv") # Data export from prod 
full_data <- read_excel("2025-26 - Production - Materials - Sample - November sample for mailmerge excluding friendly farm.xlsx")
manual_removals <- read_excel("2025-26 - Production - Materials - manual removal records for mailmerge.xlsx")

long_full_data <- pivot_longer(
  full_data,
  cols = starts_with("location"),
  names_to = "location",
  values_to = "CPH"
)%>%
  drop_na(CPH) %>%  # Removes rows where 'score' is NA
  separate(CPH, into = c("parish", "holding"), sep = "/", convert = TRUE, remove = FALSE)


# Remove rows in submitted_data from full_dataset based on the 'parish' and holding numbers
not_submitted_yet <- anti_join(long_full_data, submitted_data, by = c("parish", "holding"))


wide_not_submitted_yet <- not_submitted_yet %>%
  select(-c(parish,holding))%>%
  pivot_wider(
    names_from = location,
    values_from = CPH
  )

## Remove list of manual records to be removed following contact from the farmers via email 
wide_not_submitted_yet <- anti_join(wide_not_submitted_yet, manual_removals, by = "brn")


#export xlsx
str1 <- "mail_merge_not_submitted_yet"
str3 <- ".xlsx"
outputname <- paste(str1, format(Sys.Date(), format="%d %b"), str3) 
write_xlsx(wide_not_submitted_yet, outputname)