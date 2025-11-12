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

# Remove matching rows from main_data
Final_survey_results <- anti_join(joined_all, removals_yes2, by = c("parish","holding")) %>%
  mutate(CropGeneral = word(Crop, 1)) %>% 
  relocate(CropGeneral, .after = Crop)

#### Processing ################################################################
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
  )

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
  )

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
  )

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
  )
descriptive_stats_Scotland_GeneralCrop <- bind_rows(
  descriptive_stats_Scotland_GeneralCrop,
  total_cereals_stats
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


production_raised_ITL225 <- Survey_results_combined %>%
  left_join(census, by = c("Region", "Crop","CropGeneral")) %>% 
  relocate(CropGeneral, .after = Crop) %>% 
  relocate(census_area, .after = survey_area)


# Step 1: Create a lookup table of Scotland yields by Crop
scotland_yields <- production_raised_ITL225 %>%
  filter(Region == "Scotland") %>%
  select(Crop, CropGeneral, yield_scotland = yield)

# Step 2: Join Scotland yields to all rows
production_raised_ITL225 <- production_raised_ITL225 %>%
  left_join(scotland_yields, by = c("Crop", "CropGeneral")) %>%
  mutate(
    raised_yield = if_else(`Number of returns` < 5, yield_scotland, yield),
    production_raised = raised_yield * census_area
  )


Final_results <- production_raised_ITL225 %>%
  mutate(
    Area = census_area,
    Production = production_raised,
    Yield = Production / Area
  ) %>%
  select(Region, Crop, CropGeneral, Area, Production, Yield) %>%
  distinct(Region, Crop, CropGeneral, .keep_all = TRUE)





