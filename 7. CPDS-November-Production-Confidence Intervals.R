### Confidence intervals ###
# # Define target crops
# target_crops <- c("Total_cereals", "Barley S", "Barley W", "Wheat", "Oats", "OSRape")
# 
# # Filter and summarize
# summary_table <- production_raised_results %>%
#   filter(Crop %in% target_crops,  Region == "Scotland") %>%
#   left_join(descriptive_stats_combined, by = c("Crop", "Region")) %>%
#   mutate(
#     se_yield = sd_yield / sqrt(returns_descriptive),
#     se_production = se_yield * census_area,
#     ci_limit = 1.96 * se_production,
#     ci_percent = (1.96 * se_production / production_raised) * 100,
#     sampling_percent = (`Number of returns` / `Number of holdings`) * 100
#   ) %>%
#   select(
#     Crop,
#     `Number of holdings`,
#     `Number of returns`,
#     sampling_percent,
#     production_raised,
#     ci_limit,
#     ci_percent
#   ) %>%
#   rename(
#     `Sampling %` = sampling_percent,
#     `Production raised` = production_raised,
#     `Confidence limit` = ci_limit,
#     `Confidence %` = ci_percent
#   )

target_crops <- c("Total_cereals", "Barley S", "Barley W", "Wheat", "Oats", "OSRape")

# Step 1: Holdings from census (Scotland only)
scotland_holdings <- census %>%
  filter(Region == "Scotland", Crop %in% target_crops) %>%
  select(Crop, Holdings = `Number of holdings`)

# Step 2: Sample size + production from Final_results
scotland_results <- Final_results %>%
  filter(Region == "Scotland", Crop %in% target_crops) %>%
  select(Crop, `Number of returns`, Production)

# Step 3: Confidence limits from descriptive stats
scotland_stats <- descriptive_stats_combined %>%
  filter(Region == "Scotland", Crop %in% target_crops) %>%
  mutate(
    SE_production = sd_production / sqrt(returns_descriptive),
    CI_lower = mean_production - 1.96 * SE_production,
    CI_upper = mean_production + 1.96 * SE_production,
    CI_pct   = (1.96 * SE_production / mean_production) * 100
  ) %>%
  select(Crop, SE_production, CI_lower, CI_upper, CI_pct)

# Step 4: Merge everything into summary table
summary_table <- scotland_results %>%
  left_join(scotland_holdings, by = "Crop") %>%
  left_join(scotland_stats, by = "Crop") %>%
  mutate(
    Sampling_pct = (`Number of returns` / Holdings) * 100
  ) %>%
  select(Crop, Holdings, `Number of returns`, Sampling_pct,
         Production, CI_lower, CI_upper, CI_pct)

view(summary_table)
