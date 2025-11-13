##Once QA is complete read back in yield_outliers_summary with Final Decision filled in manually for outliers
## remove FF, wholecropped and outliers from data 
Finalised_removals <-  read_excel("2025-26 - November - Production - Data - QA - Removals (FF, WC and yield Outliers) - 13 November.xlsx")

#Filter removals to only rows where decision == "REMOVE"
removals_yes <- Finalised_removals %>%
  filter(`Final decision` == "remove")%>%
  select(parish, holding, Region, Crop, Wholecrop, reason, `Final decision`)


# Remove matching rows from main_data
Final_survey_results <- anti_join(joined_all, removals_yes, by = c("parish","holding")) %>%
  mutate(CropGeneral = word(Crop, 1)) %>% 
  relocate(CropGeneral, .after = Crop)

# filename appropriate for data upload to erdm
str9 <- " - November - Data - Raw Data - Final production sample - "
outputname4 <- paste(
  crop_year,
  str9,
  format(Sys.Date(), "%d %B"),
  str7,
  sep = ""
)
write_xlsx(Final_survey_results, outputname4)




################################################################################
#### Processing ################################################################
################################################################################

descriptive_stats_ITL225 <- Final_survey_results %>%
  filter(!is.na(Yield), !is.na(Area), !is.na(Production)) %>%  # Remove rows with missing values
  group_by(Crop, Region) %>%
  summarise(
    `Number of returns` = n(),
    
    `min yield` = min(Yield, na.rm = TRUE),
    `max yield` = max(Yield, na.rm = TRUE),
    `range yield` = max(Yield, na.rm = TRUE) - min(Yield, na.rm = TRUE),
    `mean yield` = mean(Yield, na.rm = TRUE),
    `sd yield` = sd(Yield, na.rm = TRUE),
    
    
    min_area = min(Area, na.rm = TRUE),
    max_area = max(Area, na.rm = TRUE),
    range_area = max(Area, na.rm = TRUE) - min(Area, na.rm = TRUE),
    mean_area = mean(Area, na.rm = TRUE),
    sd_area = sd(Area, na.rm = TRUE),
    
    min_production = min(Production, na.rm = TRUE),
    max_production = max(Production, na.rm = TRUE),
    range_production = max(Production, na.rm = TRUE) - min(Production, na.rm = TRUE),
    mean_production = mean(Production, na.rm = TRUE),
    sd_production = sd(Production, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(CropGeneral = word(Crop, 1)) %>%
  relocate(CropGeneral, .after = Crop)

descriptive_stats_Scotland <- Final_survey_results %>%
  filter(!is.na(Yield), !is.na(Area), !is.na(Production)) %>%
  group_by(Crop) %>%
  summarise(
    `Number of returns` = n(),
    
    `min yield` = min(Yield, na.rm = TRUE),
    `max yield` = max(Yield, na.rm = TRUE),
    `range yield` = max(Yield, na.rm = TRUE) - min(Yield, na.rm = TRUE),
    `mean yield` = mean(Yield, na.rm = TRUE),
    `sd yield` = sd(Yield, na.rm = TRUE),
    
    min_area = min(Area, na.rm = TRUE),
    max_area = max(Area, na.rm = TRUE),
    range_area = max(Area, na.rm = TRUE) - min(Area, na.rm = TRUE),
    mean_area = mean(Area, na.rm = TRUE),
    sd_area = sd(Area, na.rm = TRUE),
    
    min_production = min(Production, na.rm = TRUE),
    max_production = max(Production, na.rm = TRUE),
    range_production = max(Production, na.rm = TRUE) - min(Production, na.rm = TRUE),
    mean_production = mean(Production, na.rm = TRUE),
    sd_production = sd(Production, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(CropGeneral = word(Crop, 1),
         Region = "Scotland") %>%
  relocate(CropGeneral, .after = Crop)

descriptive_stats_Scotland_GeneralCrop <- Final_survey_results %>%
  filter(!is.na(Yield), !is.na(Area), !is.na(Production)) %>%
  group_by(CropGeneral) %>%
  summarise(
    `Number of returns` = n(),
    
    `min yield` = min(Yield, na.rm = TRUE),
    `max yield` = max(Yield, na.rm = TRUE),
    `range yield` = max(Yield, na.rm = TRUE) - min(Yield, na.rm = TRUE),
    `mean yield` = mean(Yield, na.rm = TRUE),
    `sd yield` = sd(Yield, na.rm = TRUE),
    
    min_area = min(Area, na.rm = TRUE),
    max_area = max(Area, na.rm = TRUE),
    range_area = max(Area, na.rm = TRUE) - min(Area, na.rm = TRUE),
    mean_area = mean(Area, na.rm = TRUE),
    sd_area = sd(Area, na.rm = TRUE),
    
    min_production = min(Production, na.rm = TRUE),
    max_production = max(Production, na.rm = TRUE),
    range_production = max(Production, na.rm = TRUE) - min(Production, na.rm = TRUE),
    mean_production = mean(Production, na.rm = TRUE),
    sd_production = sd(Production, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(Crop = word(CropGeneral, 1),
         Region = "Scotland") %>%
  relocate(Crop, .before = CropGeneral)

total_cereals_stats <- Final_survey_results %>%
  filter(CropGeneral %in% c("Wheat", "Barley", "Oats"),
         !is.na(Yield), !is.na(Area), !is.na(Production)) %>%
  summarise(
    CropGeneral = "Total_cereals",
    `Number of returns` = n(),
    `min yield` = min(Yield, na.rm = TRUE),
    `max yield` = max(Yield, na.rm = TRUE),
    `range yield` = max(Yield, na.rm = TRUE) - min(Yield, na.rm = TRUE),
    `mean yield` = mean(Yield, na.rm = TRUE),
    `sd yield` = sd(Yield, na.rm = TRUE),
    min_area = min(Area, na.rm = TRUE),
    max_area = max(Area, na.rm = TRUE),
    range_area = max(Area, na.rm = TRUE) - min(Area, na.rm = TRUE),
    mean_area = mean(Area, na.rm = TRUE),
    sd_area = sd(Area, na.rm = TRUE),
    min_production = min(Production, na.rm = TRUE),
    max_production = max(Production, na.rm = TRUE),
    range_production = max(Production, na.rm = TRUE) - min(Production, na.rm = TRUE),
    mean_production = mean(Production, na.rm = TRUE),
    sd_production = sd(Production, na.rm = TRUE)
  )%>%
  mutate(Crop = word(CropGeneral, 1),
         Region = "Scotland") %>%
  relocate(Crop, .before = CropGeneral)

descriptive_stats_Scotland_GeneralCrop <- bind_rows(
  descriptive_stats_Scotland_GeneralCrop,
  total_cereals_stats
)

descriptive_stats_combined <- bind_rows(
  descriptive_stats_ITL225,
  descriptive_stats_Scotland,
  descriptive_stats_Scotland_GeneralCrop
  ) %>%
  group_by(Region, Crop, CropGeneral) %>%
  slice(1) %>%
  ungroup()%>%
  rename(
    returns_descriptive = `Number of returns`,
    sd_yield = `sd yield`
  )


################################################################################
Survey_results_ITL225 <- Final_survey_results %>%
  filter(!is.na(Area), !is.na(Production)) %>%
  group_by(Crop, Region) %>%
  summarise(
    `Number of returns` = n(),
    survey_area = sum(Area, na.rm = TRUE),
    total_production = sum(Production, na.rm = TRUE),
    yield = total_production / survey_area,
    .groups = "drop"
  )%>%
  mutate(CropGeneral = word(Crop, 1))


Survey_results_Scotland <- Final_survey_results %>%
  filter(!is.na(Area), !is.na(Production)) %>%
  group_by(Crop) %>%
  summarise(
    `Number of returns` = n(),
    survey_area = sum(Area, na.rm = TRUE),
    total_production = sum(Production, na.rm = TRUE),
    yield = total_production / survey_area,
    .groups = "drop"
  )  %>%
  mutate(Region = "Scotland") %>% 
  relocate(Region, .after = Crop)%>%
  mutate(CropGeneral = word(Crop, 1))

Survey_results_Scotland_CropGeneral <- Final_survey_results %>%
  filter(!is.na(Area), !is.na(Production)) %>%
  group_by(CropGeneral) %>%
  summarise(
    `Number of returns` = n(),
    survey_area = sum(Area, na.rm = TRUE),
    total_production = sum(Production, na.rm = TRUE),
    yield = total_production / survey_area,
    .groups = "drop"
  ) %>%
  mutate(Region = "Scotland") %>% 
  relocate(Region, .after = CropGeneral)

total_cereals <- Final_survey_results %>%
  filter(CropGeneral %in% c("Wheat", "Barley", "Oats"),
         !is.na(Area), !is.na(Production)) %>%
  summarise(
    CropGeneral = "Total_cereals",
    `Number of returns` = n(),
    survey_area = sum(Area, na.rm = TRUE),
    total_production = sum(Production, na.rm = TRUE),
    yield = total_production / survey_area
  ) %>%
  mutate(Region = "Scotland") %>% 
  relocate(Region, .after = CropGeneral)

Survey_results_Scotland_CropGeneral <- bind_rows(
  Survey_results_Scotland_CropGeneral,
  total_cereals
  ) %>% 
  rename(Crop = CropGeneral)%>%
  mutate(CropGeneral = word(Crop, 1))

Survey_results_combined <- bind_rows(
  Survey_results_ITL225,
  Survey_results_Scotland,
  Survey_results_Scotland_CropGeneral
  ) %>%
  group_by(Region, Crop, CropGeneral) %>%
  slice(1) %>%
  ungroup()


################################################################################

census <- june_census %>% 
  rename(census_area = "Area") %>% 
  mutate(source = "census")%>%
  mutate(Region = if_else(str_starts(Region, "M"),
                          paste0("TL", Region),
                          Region))%>% 
  select(Region, Crop, CropGeneral, census_area, `Number of holdings`)


# Step 0: Define target regions
target_regions <- c("TLM0", "TLM1", "TLM2", "TLM3", "TLM5", "TLM9")

# Step 1: Prepare base dataset
production_raised_ITL225 <- Survey_results_combined %>%
  left_join(census, by = c("Region", "Crop", "CropGeneral")) %>%
  distinct(Crop, Region, CropGeneral, .keep_all = TRUE) %>%
  relocate(CropGeneral, .after = Crop) %>%
  relocate(census_area, .after = survey_area)

# Step 2: Get fallback yields from Scotland
scotland_yields <- production_raised_ITL225 %>%
  filter(Region == "Scotland") %>%
  select(Crop, CropGeneral, `Number of returns`, yield) %>%
  rename(yield_scotland = yield, returns_scotland = `Number of returns`)

# Step 3: Get fallback yields from CropGeneral in Scotland
scotland_general_yields <- production_raised_ITL225 %>%
  filter(Region == "Scotland", Crop == CropGeneral) %>%
  select(CropGeneral, `Number of returns`, yield) %>%
  rename(yield_scotland_general = yield, returns_scotland_general = `Number of returns`)

# Step 4: Join fallback yields
production_raised_ITL225_2 <- production_raised_ITL225 %>%
  left_join(scotland_yields, by = c("Crop", "CropGeneral")) %>%
  mutate(
    threshold = if_else(Crop == "OSRape S", 3, 5),
    
    raised_yield = case_when(
      Region %in% target_regions & `Number of returns` < threshold ~ yield_scotland,
      TRUE ~ yield
    ),
    
    Imputed = case_when(
      Region %in% target_regions & `Number of returns` < threshold & returns_scotland >= threshold ~ "Yes Scotland level",
      Region %in% target_regions & `Number of returns` < threshold & returns_scotland < threshold ~ "Yes Scotland level (low returns)",
      TRUE ~ "No ITL225 level"
    ),
    
    production_raised = raised_yield * census_area
  )

production_raised <- production_raised_ITL225_2 %>%
  mutate(
    Area = census_area,
    Production = production_raised,
    Yield = Production / Area
  ) %>%
  select(Region, Crop, CropGeneral, Area, Production, Yield, Imputed) %>%
  distinct(Region, Crop, CropGeneral, .keep_all = TRUE)

##############################################################################
# Define the crops of interest
target_crops_DEFRA <- c("Wheat","Barley W", "Barley S", "Oats", "OSRape")

# Filter and select relevant columns
DEFRA_export_table <- production_raised %>%
  filter(Region == "Scotland", Crop %in% target_crops_DEFRA) %>%
  select(Crop, Area, Yield, Production)

# Optional: Export to CSV
write.csv(DEFRA_export_table, "scotland_crop_summary.csv", row.names = FALSE)


