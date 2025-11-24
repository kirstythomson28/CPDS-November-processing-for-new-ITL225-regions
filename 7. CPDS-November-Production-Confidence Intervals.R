### Confidence intervals ###
target_crops1 <- c("Total_cereals", "Barley S", "Barley W", "Wheat", "Oats", "OSRape")
target_crops2 <- c("Total_cereals", "Barley S", "Barley W", "Total_Wheat", "Total_Oats", "Total_OSRape")

# Step 1: Holdings from census (Scotland only)
scotland_holdings <- census %>%
  filter(Region == "Scotland", Crop %in% target_crops1) %>%
  select(Crop, Holdings = `Number of holdings`) %>% 
  mutate(Crop = case_when(
    Crop == "Wheat"   ~ "Total_Wheat",
    Crop == "Oats"    ~ "Total_Oats",
    Crop == "OSRape"  ~ "Total_OSRape",
    TRUE ~ Crop
  )) %>% 
  distinct(Crop, .keep_all = TRUE)


# Step 2: Sample size + production from Final_results
scotland_results <- Final_results %>%
  filter(Region == "Scotland", Crop %in% target_crops2) %>%
  select(Crop, `Number of returns`, Production)

# Step 3: Confidence limits from descriptive stats
scotland_stats <- descriptive_stats_combined %>%
  filter(Region == "Scotland", Crop %in% target_crops1) %>%
  mutate(
    SE_production = sd_production / sqrt(returns_descriptive),
    CI_lower = mean_production - 1.96 * SE_production,
    CI_upper = mean_production + 1.96 * SE_production,
    CI_limits = 1.96 * SE_production,
    CI_pct   = (1.96 * SE_production / mean_production) * 100,
    Crop = case_when(
      Crop == "Wheat"   ~ "Total_Wheat",
      Crop == "Oats"    ~ "Total_Oats",
      Crop == "OSRape"  ~ "Total_OSRape",
      TRUE              ~ Crop
    )
  ) %>%
  select(Crop, SE_production, CI_lower, CI_upper, CI_limits, CI_pct) %>%
  distinct(Crop, .keep_all = TRUE)



# Step 4: Merge everything into summary table
summary_table <- scotland_results %>%
  left_join(scotland_holdings, by = "Crop") %>%
  left_join(scotland_stats, by = "Crop") %>%
  mutate(
    Crop = case_when(
      Crop == "Total_cereals"  ~ "Total Cereals",
      Crop == "Barley S"   ~ "Spring Barley",
      Crop == "Barley W"    ~ "Winter Barley",
      Crop == "Total_Wheat"  ~ "Wheat",
      Crop == "Total_Oats"  ~ "Oats",
      Crop == "Total_OSRape"  ~ "Oilseed Rape",
      TRUE              ~ Crop
    ),
    `Number of Holdings (June Census)` = Holdings,
    `Sample Size`=`Number of returns`,
    `Sampling %` = round((`Number of returns` / Holdings) * 100,2),
    `Production ('000 tonnes)`= round(Production/1000,0),
    `Confidence Limits ('000 tonnes)`= CI_limits,
    `Confidence Limits (%)`= CI_pct
  ) %>%
  select(Crop, `Number of Holdings (June Census)`, `Sample Size`, `Sampling %`,
         `Production ('000 tonnes)`, `Confidence Limits ('000 tonnes)`, `Confidence Limits (%)`)

view(summary_table)
