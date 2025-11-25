##Once QA is complete read back in yield_outliers_summary with Final Decision filled in manually for outliers
## remove FF, wholecropped and outliers from data 
Finalised_removals <- read_excel(
  file.path("November results",
            "2025-26 - November - Production - Data - QA - Removals (FF, WC and yield Outliers) - 24 November 08-56.xlsx"))

#Filter removals to only rows where decision == "REMOVE"adj_yield
removals_yes <- Finalised_removals %>%
  filter(`Final decision` == "remove")%>%
  select(parish, holding, Region, Crop, Wholecrop, reason, `Final decision`)


# Remove matching rows from main_data
Final_survey_results <- anti_join(joined_all, removals_yes, by = c("parish","holding")) %>%
  mutate(
    CropGeneral = word(Crop, 1),
    Yield = if_else(
      is.na(Yield) & !is.na(Production) & !is.na(Area) & Area != 0,
      Production / Area,
      Yield
    ),
    # Step 1: ensure numeric moisture
    Moisture_content = as.numeric(Moisture_content),
    standard_moisture = if_else(Crop %in% c("OSRape W", "OSRape S"), 9.0, 14.5),
    
    # Step 2: adjustment logic using coalesce
    adjustment = coalesce(
      (100 - Moisture_content) / (100 - standard_moisture),
      1.0   # default when Moisture_content is NA
    ),
    
    # Step 3: adjusted production
    adj_production = Production * adjustment,
    
    # Step 4: adjusted yield
    adj_yield = if_else(Area != 0 & !is.na(Area), adj_production / Area, NA_real_)
  ) %>%
  relocate(CropGeneral, .after = Crop)




# filename appropriate for data upload to erdm
str9 <- " - November - Production - Data - Raw Data - Final production sample - "
outputname4 <- paste(
  crop_year,
  str9,
  format(Sys.Date(), "%d %B"),
  str7,
  sep = ""
)
write_xlsx(
  Final_survey_results,
  file.path("November results", outputname4)
)




################################################################################
#### Processing ################################################################
################################################################################

descriptive_stats_ITL225 <- Final_survey_results %>%
  filter(!is.na(adj_yield)) %>%
  group_by(Crop, Region) %>%
  summarise(
    `Number of returns` = n(),
    `min yield` = min(adj_yield, na.rm = TRUE),
    `max yield` = max(adj_yield, na.rm = TRUE),
    `range yield` = max(adj_yield, na.rm = TRUE) - min(adj_yield, na.rm = TRUE),
    `mean yield` = mean(adj_yield, na.rm = TRUE),
    `sd yield` = sd(adj_yield, na.rm = TRUE),
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
  filter(!is.na(adj_yield), !is.na(Area), !is.na(Production)) %>%
  group_by(Crop) %>%
  summarise(
    `Number of returns` = n(),
    
    `min yield` = min(adj_yield, na.rm = TRUE),
    `max yield` = max(adj_yield, na.rm = TRUE),
    `range yield` = max(adj_yield, na.rm = TRUE) - min(adj_yield, na.rm = TRUE),
    `mean yield` = mean(adj_yield, na.rm = TRUE),
    `sd yield` = sd(adj_yield, na.rm = TRUE),
    
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
  filter(!is.na(adj_yield), !is.na(Area), !is.na(Production)) %>%
  group_by(CropGeneral) %>%
  summarise(
    `Number of returns` = n(),
    
    `min yield` = min(adj_yield, na.rm = TRUE),
    `max yield` = max(adj_yield, na.rm = TRUE),
    `range yield` = max(adj_yield, na.rm = TRUE) - min(adj_yield, na.rm = TRUE),
    `mean yield` = mean(adj_yield, na.rm = TRUE),
    `sd yield` = sd(adj_yield, na.rm = TRUE),
    
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
         !is.na(adj_yield), !is.na(Area), !is.na(Production)) %>%
  summarise(
    CropGeneral = "Total_cereals",
    `Number of returns` = n(),
    `min yield` = min(adj_yield, na.rm = TRUE),
    `max yield` = max(adj_yield, na.rm = TRUE),
    `range yield` = max(adj_yield, na.rm = TRUE) - min(adj_yield, na.rm = TRUE),
    `mean yield` = mean(adj_yield, na.rm = TRUE),
    `sd yield` = sd(adj_yield, na.rm = TRUE),
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
  # slice(1) %>%
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
################################################################################
census <- june_census %>% 
  rename(census_area = "Area") %>% 
  mutate(source = "census") %>%
  mutate(Region = if_else(str_starts(Region, "M"),
                          paste0("TL", Region),
                          Region)) %>% 
  select(Region, Crop, CropGeneral, census_area, `Number of holdings`)

# --- Step 1: Scotland-level yields (for fallback) ---
scotland_yields <- Survey_results_combined %>%
  filter(Region == "Scotland") %>%
  select(Crop, scotland_yield = yield)

# --- Step 2: Merge census with survey results (exclude Scotland rows) ---
Survey_with_census <- census %>%
  filter(Region != "Scotland") %>%
  left_join(Survey_results_combined, by = c("Crop","CropGeneral","Region")) %>%
  left_join(scotland_yields, by = "Crop")

# --- Step 3: Compute production_raised for individual regions (keep census_area) ---
production_raised_results <- Survey_with_census %>%
  mutate(
    production_raised = case_when(
      Crop == "OSRape S" & !is.na(`Number of returns`) & `Number of returns` >= 3 ~ yield * census_area,
      Crop != "OSRape S" & !is.na(`Number of returns`) & `Number of returns` >= 5 ~ yield * census_area,
      TRUE ~ scotland_yield * census_area
    )
  ) %>%
  select(Crop, CropGeneral, Region, `Number of returns`, census_area, production_raised)

# --- Step 4: Scotland totals per crop ---
scotland_totals <- production_raised_results %>%
  group_by(Crop, CropGeneral) %>%
  summarise(
    `Number of returns` = sum(`Number of returns`, na.rm = TRUE),
    census_area         = sum(census_area, na.rm = TRUE),
    production_raised   = sum(production_raised, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Region = "Scotland",
    raised_yield = production_raised / census_area
  )

production_raised_results <- bind_rows(production_raised_results, scotland_totals)

# --- Step 5: Scotland totals per CropGeneral ---
scotland_cropgeneral_totals <- scotland_totals %>%
  group_by(CropGeneral) %>%
  summarise(
    `Number of returns` = sum(`Number of returns`, na.rm = TRUE),
    census_area         = sum(census_area, na.rm = TRUE),
    production_raised   = sum(production_raised, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Crop        = paste0("Total_", CropGeneral),
    Region      = "Scotland",
    raised_yield = production_raised / census_area
  )

production_raised_results <- bind_rows(production_raised_results, scotland_cropgeneral_totals)

# --- Step 6: Scotland Total_cereals (from Total_Wheat, Total_Barley, Total_Oats) ---
total_cereals <- scotland_cropgeneral_totals %>%
  filter(CropGeneral %in% c("Wheat","Barley","Oats")) %>%
  summarise(
    `Number of returns` = sum(`Number of returns`, na.rm = TRUE),
    census_area         = sum(census_area, na.rm = TRUE),
    production_raised   = sum(production_raised, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Crop        = "Total_cereals",
    CropGeneral = "Total_cereals",
    Region      = "Scotland",
    raised_yield = production_raised / census_area
  )

production_raised_results <- bind_rows(production_raised_results, total_cereals)

###############################################################################

### Final results 

# --- Step: Join Scotland raised_yield by Crop ---
# --- Step: Join Scotland raised_yield by Crop ---
scotland_crop_yields <- production_raised_results %>%
  filter(Region == "Scotland") %>%
  select(Crop, CropGeneral, scotland_raised_yield = raised_yield)

Final_results <- Survey_with_census %>%
  left_join(scotland_crop_yields, by = c("Crop","CropGeneral")) %>%
  mutate(
    Area = census_area,
    
    # Production logic
    Production = case_when(
      Crop == "OSRape S" & !is.na(`Number of returns`) & `Number of returns` >= 3 ~ yield * census_area,
      Crop != "OSRape S" & !is.na(`Number of returns`) & `Number of returns` >= 5 ~ yield * census_area,
      TRUE ~ scotland_raised_yield * census_area
    ),
    
    # Yield logic
    Yield = case_when(
      Crop == "OSRape S" & !is.na(`Number of returns`) & `Number of returns` >= 3 ~ yield,
      Crop != "OSRape S" & !is.na(`Number of returns`) & `Number of returns` >= 5 ~ yield,
      TRUE ~ scotland_raised_yield
    ),
    
    # Flag for imputation
    Imputed = case_when(
      Crop == "OSRape S" & !is.na(`Number of returns`) & `Number of returns` >= 3 ~ "NO",
      Crop != "OSRape S" & !is.na(`Number of returns`) & `Number of returns` >= 5 ~ "NO",
      TRUE ~ "YES imputed at scotland level"
    )
  ) %>%
  select(Crop, CropGeneral, Region, `Number of returns`, Area, Production, Yield, Imputed)

# --- Step: Scotland totals from regional rows (TLM0:TLM9) ---
scotland_totals_from_regions <- Final_results %>%
  filter(grepl("^TLM[0-9]$", Region)) %>%   # keep only TLM0–TLM9 regions
  group_by(Crop, CropGeneral) %>%
  summarise(
    Area       = sum(Area, na.rm = TRUE),
    Production = sum(Production, na.rm = TRUE),
    `Number of returns` = sum(`Number of returns`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Region = "Scotland",
    Yield  = Production / Area,
    Imputed = "No"   # optional flag to distinguish these totals
  )

# Append Scotland totals to Final_results
Final_results <- bind_rows(Final_results, scotland_totals_from_regions)

# --- Step: Scotland crop totals by CropGeneral ---
scotland_crop_totals <- Final_results %>%
  filter(Region == "Scotland") %>%              # only Scotland rows
  group_by(CropGeneral) %>%
  summarise(
    Area       = sum(Area, na.rm = TRUE),
    Production = sum(Production, na.rm = TRUE),
    `Number of returns` = sum(`Number of returns`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Crop   = paste0("Total_", CropGeneral),     # label as Total_<CropGeneral>
    Region = "Scotland",
    Yield  = Production / Area,
    Imputed = "NO"   # optional flag
  )

# Append crop totals to Final_results
Final_results <- bind_rows(Final_results, scotland_crop_totals)

# --- Step: Scotland Total_cereals from Total_Wheat, Total_Barley, Total_Oats ---
total_cereals <- Final_results %>%
  filter(Region == "Scotland", Crop %in% c("Total_Wheat","Total_Barley","Total_Oats")) %>%
  summarise(
    Area       = sum(Area, na.rm = TRUE),
    Production = sum(Production, na.rm = TRUE),
    `Number of returns` = sum(`Number of returns`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Crop        = "Total_cereals",
    CropGeneral = "Total_cereals",
    Region      = "Scotland",
    Yield       = Production / Area,
    Imputed     = "NO"
  )

# Append Total_cereals to Final_results
Final_results <- bind_rows(Final_results, total_cereals)

# filename appropriate for data upload to erdm
str9 <- " - November - Production - Data - Final results - "
outputname4 <- paste(
  crop_year,
  str9,
  format(Sys.Date(), "%d %B"),
  str7,
  sep = ""
)
write_xlsx(
  Final_results,
  file.path("November results", outputname4)
)


##############################################################################
# Define the crops of interest
target_crops_DEFRA <- c("Total_Wheat","Barley W", "Barley S", "Total_Oats", "Total_OSRape")

# Filter and select relevant columns
DEFRA_export_table <- Final_results %>%
  filter(Region == "Scotland", Crop %in% target_crops_DEFRA) %>%
  select(Crop, Area, Yield, Production)


# filename appropriate for data upload to erdm
str9 <- " - November - Production - Data - DEFRA Export table - "
outputname4 <- paste(
  crop_year,
  str9,
  format(Sys.Date(), "%d %B"),
  str7,
  sep = ""
)
write_xlsx(
  DEFRA_export_table,
  file.path("November results", outputname4)
)

###### CH_data

Newest_year_CH_data <- Final_results %>% 
  mutate(Year = 2025) %>%
  filter(Crop %in% c("Total_Oats", "Total_OSRape", "Barley S",
                     "Total_cereals","Total_Wheat","Barley W", "Total_Barley"),
         Region == "Scotland") %>% 
  pivot_wider(
    id_cols = Year,   # keep Year as a column
    names_from = Crop,
    values_from = c(Production, Area, Yield),
    names_glue = "{Crop}_{.value}"   # creates names like Oats_Production, Oats_Area, Oats_Yield
  ) %>%
  rename(
    Oats_Production      = Total_Oats_Production,
    OSR_Production       = Total_OSRape_Production,
    S_Barley_Production  = `Barley S_Production`,
    W_Barley_Production  = `Barley W_Production`,
    Barley_Production    = Total_Barley_Production,
    Cereals_Production   = Total_cereals_Production,
    Wheat_Production     = Total_Wheat_Production,

    Oats_Area            = Total_Oats_Area,
    OSR_Area             = Total_OSRape_Area,
    S_Barley_Area        = `Barley S_Area`,
    W_Barley_Area        = `Barley W_Area`,
    Barley_Area          = Total_Barley_Area,
    Cereals_Area         = Total_cereals_Area,
    Wheat_Area           = Total_Wheat_Area,

    Oats_Yield           = Total_Oats_Yield,
    OSR_Yield            = Total_OSRape_Yield,
    S_Barley_Yield       = `Barley S_Yield`,
    W_Barley_Yield       = `Barley W_Yield`,
    Barley_Yield         = Total_Barley_Yield,
    Cereals_Yield        = Total_cereals_Yield,
    Wheat_Yield          = Total_Wheat_Yield
  ) %>%
  mutate(across(ends_with("_Production"), as.numeric),
         across(ends_with("_Area"), as.numeric),
         across(ends_with("_Yield"), as.numeric))


ch_data <- bind_rows(ch_data, Newest_year_CH_data)
