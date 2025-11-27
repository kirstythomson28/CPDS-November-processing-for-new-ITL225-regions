## QA of November production results
##set expected bounds for the yield of the crop 

low_bounds <- c(
  "Wheat" = 4, 
  "Barley S" = 3, 
  "Barley W" = 3,
  "Oats S" = 3, 
  "Oats W" = 3, 
  "OSRape S" = 1, 
  "OSRape W" = 1.5
)

high_bounds <- c(
  "Wheat" = 12, 
  "Barley S" = 10, 
  "Barley W" = 11,
  "Oats S" = 11, 
  "Oats W" = 11, 
  "OSRape S" = 4.5, 
  "OSRape W" = 6
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
    CPH = paste0(str_pad(CPH_part1, 3, pad = "0"), "/", str_pad(CPH_part2, 4, pad = "0"))
    ) %>%
  select(-c(CPH_part1,CPH_part2,`yield flag low`, `yield flag high`,ITL225CD)) %>% 
  mutate(reason = "manual outlier",
         `Final decision`="")

QA_production_nov_emails <- left_join(
  QA_production_nov,
  Sample %>% select(parish, holding, primary_email),
  by = c("parish", "holding")
  ) %>% 
  mutate(Crop = case_when(
             Crop == "Barley S" ~ "Spring barley",
             Crop == "Barley W" ~ "Winter barley",
             Crop == "Oats S" ~ "Spring oats",
             Crop == "Oats W" ~ "Winter oats",
             Crop == "OSRape S" ~ "Spring oilseed rape",
             Crop == "OSRape W" ~ "Winter oilseed rape",
             TRUE ~ Crop))

Yield_QA_Log_emails <- Yield_QA_Log %>% 
  mutate(Crop = case_when(
    Crop == "Barley S" ~ "Spring barley",
    Crop == "Barley W" ~ "Winter barley",
    Crop == "Oats S" ~ "Spring oats",
    Crop == "Oats W" ~ "Winter oats",
    Crop == "OSRape S" ~ "Spring oilseed rape",
    Crop == "OSRape W" ~ "Winter oilseed rape",
    TRUE ~ Crop))

QA_production_nov_emails_new <- anti_join(
  QA_production_nov_emails, Yield_QA_Log_emails,   
  by = c("CPH", "parish", "holding", "Region", "Crop"))

view(QA_production_nov_emails_new)

#export xlsx
# filename appropriate for data upload to erdm
str5 <- " - Production QA as unexpected yield (manual bounds) - "
outputname <- paste(
  crop_year,
  str5,
  format(Sys.time(), "%d %B %H-%M"),  
  str7,
  sep = ""
)
write_xlsx(
  QA_production_nov_emails_new,
  file.path("QA files", outputname)
)
write_xlsx(
  QA_production_nov_emails_new,
  file.path("Mail merge", outputname)
)

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
    arrange(adj_yield) %>%
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
    "Adjusted Yield Spread{if (!is.null(crop_filter)) paste0(': ', crop_filter) else ''}{if (!is.null(region_filter)) paste0(' - ', region_filter) else ''}"
  )
  
  # ---- Build plot ----
  p <- ggplot(filtered_data, aes(x = index, y = adj_yield, color = is_outlier)) +
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
    filename = file.path("QA files", file_name),
    plot = p,
    device = "svg",
    width = 8,
    height = 6
  )
  
  
  message(glue("✅ Saved plot: {file_name}"))
  
  # Return outliers
  outlier_data <- filtered_data %>%
    filter(is_outlier) %>%
    select(CPH,parish, holding, Region, Crop, Area, Production, Moisture_content, Yield, adj_yield, adj_production)
  
  return(outlier_data)
}


# ---- Generate all plots and collect all outliers ----
crops <- c("Wheat", "Barley S", "Barley W", "Oats S", "Oats W", "OSRape S", "OSRape W")
regions <- c("TLM0", "TLM1", "TLM2", "TLM5", "TLM9")

# Collect outliers from all plots
all_outliers <- map2_dfr(
  rep(crops, each = length(regions)),
  rep(regions, times = length(crops)),
  ~ plot_yield_spread(joined_all, crop_filter = .x, region_filter = .y)
) %>% 
  mutate(reason="survey outlier",
         Wholecrop = "NO",
         `Final decision`="")

combined_outliers <- QA_production_nov %>%
  select(CPH,parish, holding, Region, Crop, Area, Production, Moisture_content, 
         Yield, adj_yield, adj_production, reason, Wholecrop, `Final decision`) %>%
  bind_rows(
    all_outliers %>%
      select(CPH,parish, holding, Region, Crop, Area, Production, Moisture_content,
             Yield, adj_yield, adj_production, reason, Wholecrop, `Final decision`)
  ) %>%
  group_by(parish, holding,  Region, Crop) %>%
  mutate(reason = if (n() > 1) "double outlier" else reason) %>%
  ungroup() %>%
  distinct(parish, holding,  Region, Crop, .keep_all = TRUE)


Yield_QA_Log <- Yield_QA_Log %>%
  bind_rows(
    combined_outliers %>%
      anti_join(Yield_QA_Log,
                by = c("CPH", "parish", "holding", "Region", "Crop"))
  )

#export xlsx
# filename appropriate for data upload to erdm
str5 <- " - November - Production - Data - QA - LOG"
outputname_removals <- paste(
  crop_year,
  str5,
  str7,
  sep = ""
)

write_xlsx(
  Yield_QA_Log,
  file.path("Setup documents", outputname_removals)
)

removals_FF_WC_Outliers <- removals_FF_WC %>%
  bind_rows(
    Yield_QA_Log %>%
      select(CPH, parish, holding, Region, Crop, Yield, adj_yield, reason, Wholecrop, `Final decision`)
  )%>%
  distinct(parish, holding,  Region, Crop, .keep_all = TRUE)

#export xlsx
# filename appropriate for data upload to erdm
str5 <- " - Removals (FF, WC and yield Outliers) - "
outputname_removals <- paste(
  crop_year,
  str5,
  format(Sys.time(), "%d %B %H-%M"),
  str7,
  sep = ""
)

write_xlsx(
  removals_FF_WC_Outliers,
  file.path("QA files", outputname_removals)
)

#export xlsx
# filename appropriate for data upload to erdm
str5 <- " - Removals (FF, WC and yield Outliers) - FINAL"
outputname <- paste(
  crop_year,
  str5,
  str7,
  sep = ""
)

write_xlsx(
  removals_FF_WC_Outliers,
  file.path("QA files", outputname)
)                  