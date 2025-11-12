## remove FF, wholecropped and outliers from data 
Finalised_removals <- read_excel("2025-26 - November - Production - Data - QA - Production values flagged in QA as unexpected yield - 10 November.xlsx")
#Filter removals to only rows where decision == "REMOVE"
removals_yes <- Finalised_removals %>%
  filter(`Final decision (YES = remove, NO = Keep, CHANGE = change values)` == "REMOVE")%>%
  select(parish, holding, Region, Crop, Wholecrop) %>% 
  mutate(reason = "outlier following qa")

removals_yes2 <- bind_rows(
  removals_FF_WC,
  removals_yes)

# Step 2: Remove matching rows from main_data
Final_survey_results <- anti_join(joined_all, removals_yes2, by = c("parish","holding"))

