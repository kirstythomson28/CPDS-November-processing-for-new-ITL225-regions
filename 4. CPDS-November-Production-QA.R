## QA of November production results
##set expected bounds for the yield of the crop 

low_bounds <- c(
  "Wheat" = 4, 
  "Barley S" = 3, 
  "Barley W" = 3,
  "Oats S" = 3, 
  "Oats W" = 3, 
  "OSR S" = 1, 
  "OSR W" = 1.5
)

high_bounds <- c(
  "Wheat" = 12, 
  "Barley S" = 10, 
  "Barley W" = 11,
  "Oats S" = 11, 
  "Oats W" = 11, 
  "OSR S" = 4.5, 
  "OSR W" = 6
)

QA_production_nov <- joined_all %>%
  separate(CPH, into = c("CPH_part1", "CPH_part2"), sep = "/", remove = FALSE) %>%
  mutate(
    `yield flag low` = Yield < low_bounds[Crop],
    `yield flag high` = Yield > high_bounds[Crop],
    direction = case_when(
      `yield flag high` ~ "higher",
      `yield flag low` ~ "lower",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(`yield flag low` | `yield flag high`) %>% 
  mutate(
    Yield = round(Yield, 1),
    Area = round(Area, 2),
    Production = round(Production, 0)
  ) %>% 
  mutate(
    CPH = paste0(str_pad(CPH_part1, 3, pad = "0"), "/", str_pad(CPH_part2, 4, pad = "0")),
    Crop = case_when(
      Crop == "Barley S" ~ "Spring barley",
      Crop == "Barley W" ~ "Winter barley",
      Crop == "Oats S" ~ "Spring oats",
      Crop == "Oats W" ~ "Winter oats",
      Crop == "Oilseed Rape S" ~ "Spring oilseed rape",
      Crop == "Oilseed Rape W" ~ "Winter oilseed rape",
      TRUE ~ Crop
    )) %>% 
  select(-c(CPH_part1,CPH_part2))

QA_production_nov <- left_join(
  QA_production_nov,
  Sample %>% select(parish, holding, primary_email),
  by = c("parish", "holding")
)


#export xlsx
# filename appropriate for data upload to erdm
str5 <- " - November - Production - Data - QA - Production values flagged in QA as unexpected yield - "
outputname <- paste(
  crop_year,
  str5,
  format(Sys.Date(), "%d %B"),
  str7,
  sep = ""
)
write_xlsx(QA_production_nov, outputname)

###############################################################################
# Attempting to recreate outliers identification like in excel 
plot_yield_spread <- function(data, crop_filter = NULL, region_filter = NULL) {
  
  # Conditional filtering
  filtered_data <- data %>%
    { if (!is.null(crop_filter)) filter(., Crop == crop_filter) else . } %>%
    { if (!is.null(region_filter)) filter(., Region == region_filter) else . }
  
  if (nrow(filtered_data) == 0) {
    message(glue("⚠️ No data available for {crop_filter} - {region_filter}, skipping..."))
    return(NULL)
  }
  
  # Compute outliers + auto bounds
  filtered_data <- filtered_data %>%
    group_by(Crop, Region) %>%
    arrange(Yield) %>%
    mutate(
      index = row_number(),
      Q1 = quantile(Yield, 0.25, na.rm = TRUE),
      Q3 = quantile(Yield, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      lower_bound = Q1 - 1.5 * IQR,
      upper_bound = Q3 + 1.5 * IQR,
      is_outlier = Yield < lower_bound | Yield > upper_bound
    ) %>%
    ungroup()
  
  # ---- Extract bounds for this crop ----
  manual_low <- low_bounds[[crop_filter]]
  manual_high <- high_bounds[[crop_filter]]
  
  auto_low <- unique(filtered_data$lower_bound)
  auto_high <- unique(filtered_data$upper_bound)
  
  # ---- Create a data frame for line legend ----
  bound_lines <- tibble(
    BoundType = c("Manual Bounds", "Manual Bounds", "IQR Bounds", "IQR Bounds"),
    y = c(manual_low, manual_high, auto_low, auto_high)
  )
  
  # Create title
  title_text <- glue(
    "Yield Spread{if (!is.null(crop_filter)) paste0(': ', crop_filter) else ''}{if (!is.null(region_filter)) paste0(' - ', region_filter) else ''}"
  )
  
  # ---- Build plot ----
  p <- ggplot(filtered_data, aes(x = index, y = Yield, color = is_outlier)) +
    geom_point(size = 2, alpha = 0.7) +
    scale_color_manual(values = c("FALSE" = "#1f77b4", "TRUE" = "#d62728"), name = "Outlier") +
    
    # Add horizontal lines with legend
    geom_hline(
      data = bound_lines,
      aes(yintercept = y, linetype = BoundType),
      color = c("black", "black", "purple", "purple"),
      linewidth = 0.9
    ) +
    scale_linetype_manual(
      name = "Bound Type",
      values = c("Manual Bounds" = "solid", "IQR Bounds" = "dashed")
    ) +
    
    # Labels and formatting
    labs(
      title = title_text,
      x = "Ordered Observation",
      y = "Yield"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.box = "vertical"
    ) +
    facet_wrap(~ Crop + Region, scales = "free_y")
  
  # ---- Save SVG ----
  file_name <- glue("{crop_filter}_{region_filter}_yield_spread.svg") %>%
    gsub(" ", "_", .)
  
  ggsave(
    filename = file_name,
    plot = p,
    device = "svg",
    width = 8,
    height = 6
  )
  
  message(glue("✅ Saved plot: {file_name}"))
  
  # Return outliers
  outlier_data <- filtered_data %>%
    filter(is_outlier) %>%
    select(parish, holding, Crop, Region, Yield)
  
  return(outlier_data)
}


# ---- Generate all plots and collect all outliers ----
crops <- c("Wheat", "Barley S", "Barley W", "Oats S", "Oats W", "OSR S", "OSR W")
regions <- c("TLM0", "TLM1", "TLM2", "TLM5", "TLM9")

# Collect outliers from all plots
all_outliers <- map2_dfr(
  rep(crops, each = length(regions)),
  rep(regions, times = length(crops)),
  ~ plot_yield_spread(joined_all, crop_filter = .x, region_filter = .y)
) %>% 
  mutate(reason="outlier",
         Wholecrop = "NO",
         `Final decision`="")

removals_FF_WC_Outliers <- removals_FF_WC %>%
  bind_rows(
    all_outliers %>%
      select(parish, holding, Crop, Region, Wholecrop, reason,`Final decision`)
)

# 🔸 Save all outliers to CSV
write_csv(removals_FF_WC_Outliers, "yield_outliers_summary.csv")
message("📄 All outliers saved to 'yield_outliers_summary.csv'")
                  