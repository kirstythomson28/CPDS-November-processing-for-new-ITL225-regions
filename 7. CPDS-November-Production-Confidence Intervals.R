### Confidence intervals ###
# Define target crops
target_crops <- c("Total_cereals", "Barley S", "Barley W", "Wheat", "Oats", "OSRape")

# Filter and summarize
summary_table <- production_raised_ITL225_2 %>%
  filter(Crop %in% target_crops,  Region == "Scotland") %>%
  left_join(descriptive_stats_combined, by = c("Crop", "Region")) %>%
  mutate(
    se_yield = sd_yield / sqrt(returns_descriptive),
    se_production = se_yield * census_area,
    ci_limit = 1.96 * se_production,
    ci_percent = (1.96 * se_production / production_raised) * 100,
    sampling_percent = (`Number of returns` / `Number of holdings`) * 100
  ) %>%
  select(
    Crop,
    `Number of holdings`,
    `Number of returns`,
    sampling_percent,
    production_raised,
    ci_limit,
    ci_percent
  ) %>%
  rename(
    `Sampling %` = sampling_percent,
    `Production raised` = production_raised,
    `Confidence limit` = ci_limit,
    `Confidence %` = ci_percent
  )


