## Disposals QA ##

## QA ##
### Needs further development ###
Disposals_oct_QA <- combined_df %>%
  rename_with(.fn = ~ paste0(.x, "_oct"), .cols = Merchants_for_Malting:Total_Disposed)


Disposals_QA_reordered <- Disposals_oct_QA %>%
  select(
    CPH, parish, holding, Region, Crop,
    ends_with("_oct"),
    everything()
  )%>%
  relocate(Total_Disposed_oct, .before = Closing_Stock_Oct)

Disposals_QA_Flagged <- Disposals_QA_reordered %>%
  mutate(
    # Difference between OCt opening and (Oct closing + disposals)
    Oct_Stock_Discrepancy_Flag = ifelse(
      Opening_stock_Oct != (Closing_Stock_Oct + Total_Disposed_oct), 1, 0
    ),
    Difference_Oct_Op_Clo_Stock = ifelse(
      Oct_Stock_Discrepancy_Flag == 1, 
      Opening_stock_Oct - (Closing_Stock_Oct + Total_Disposed_oct), 0
    ),
    # Flags for unusual usage by crop type
    Wheat_Milling_flag_Oct = if_else(
      Crop == "Wheat" & Merchants_for_Milling_oct > 0.3 * Opening_stock_Oct,
      "Milling flag",
      NA_character_
    ),
    Barley_Milling_flag_Oct = if_else(
      Crop == "Barley" & Merchants_for_Milling_oct > 0.10 * Opening_stock_Oct,
      "Milling flag",
      NA_character_
    ),
    Oats_Malting_flag_Oct = if_else(
      Crop == "Oats" & Merchants_for_Malting_oct > 0.2 * Opening_stock_Oct,
      "Malting flag",
      NA_character_
    ),
  )

# Create data frame of those just with issues 
Disposals_QA_with_issues <- Disposals_QA_Flagged %>%
  filter(
      Oct_Stock_Discrepancy_Flag == 1 |
      !is.na(Wheat_Milling_flag_Oct) |
      !is.na(Barley_Milling_flag_Oct) |
      !is.na(Oats_Malting_flag_Oct)
  ) 
# To view it in RStudio (optional)
View(Disposals_QA_with_issues)



#export xlsx
# filename appropriate for data upload to erdm
str8 <- "November - Data - Raw Data - QA - November Disposals requiring QA - "
outputname4 <- paste(
  crop_year, 
  str8, 
  format(Sys.Date(), format="%d %B"), 
  str3, 
  sep = "") 

write.csv(Disposals_QA_with_issues, outputname4, row.names = FALSE)


## Data set to be used in mail merge for stock mismatch.
data <- Disposals_QA_with_issues %>%
  filter(Oct_Stock_Discrepancy_Flag == 1) %>%
  select(-c(
            "Difference_Oct_Op_Clo_Stock", 
            "Wheat_Milling_flag_Oct",
            "Barley_Milling_flag_Oct",
            "Oats_Malting_flag_Oct",
            )
         )

sample_QA <- Sample %>%
  select(parish, holding, primary_email)


# Then join to data by CPH
Qa_emails <- data %>%
  left_join(sample_QA, by = c("parish","holding"))%>%
  relocate(primary_email, .before = Region)


#export xlsx
# filename appropriate for data upload to erdm
str9 <- " - November - Data - Raw Data - QA - Emails for stock mis match - "
outputname5 <- paste(
  crop_year,
  str8,
  format(Sys.Date(), format="%d %B"), 
  str3,
  sep = "")
write.csv(Qa_emails, outputname5, row.names = FALSE)
