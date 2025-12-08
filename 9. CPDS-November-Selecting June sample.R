####### Selecting Sample for July ########
removals_cph <- removals_FF_WC_Outliers %>% 
  filter(`Final decision` == "remove")%>%
  distinct(CPH, .keep_all = TRUE)

data <- combined_df %>%
  filter(!CPH %in% removals_cph$CPH)%>% #Remove any row if CPH is in removed cph
  filter(Crop != "OSRape") #Remove OSRape

#Remove any where no crop is left
data_filtered <- data %>%
  filter(Opening_stock_Oct != "0")%>%
  filter(Closing_Stock_Oct != "0")

July_Sample <- data_filtered %>%  
  select(CPH, parish, holding) %>%      
  distinct(CPH, .keep_all = TRUE) 

### Mailmerge ###

mail_merge <- Sample_2 %>%
  filter(CPH %in% July_Sample$CPH)%>%
  select(CPH, parish, holding, brn, primary_email)

mail_merge_wide <- mail_merge %>%
  select(-parish, -holding) %>%
  group_by(brn) %>%
  mutate(location_num = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    names_from = location_num,
    values_from = CPH,
    names_prefix = "location")

July_Sample <- July_Sample %>%
  select(-CPH)

colnames(July_Sample) <- NULL


# Filename appropriate for data upload to ERDM
str6 <- " - November - Data - Formatted data output for july sample - location only - "
str3 <- ".csv"
outputname4 <- paste(
  crop_year,
  str6,
  format(Sys.Date(), "%d %B"),
  str3,
  sep = ""
)

# Save as CSV without headers
write_csv(July_Sample,
          file.path("July sample", outputname4),
          col_names = FALSE)



# Filename appropriate for data upload to ERDM
str8 <- " - November - Data - CPH removed from july sample - location only - "
outputname5 <- paste(
  crop_year,
  str8,
  format(Sys.Date(), "%d %B"),
  str7,
  sep = ""
)
write_xlsx(removals_cph, 
           file.path("July sample", outputname5)
          )

# Filename appropriate for data upload to ERDM
str9 <- " - November - Data - july sample - mail merge - "
outputname6 <- paste(
  crop_year,
  str9,
  format(Sys.Date(), "%d %B"),
  str7,
  sep = ""
)
write_xlsx(mail_merge_wide, 
           file.path("July sample", outputname6)
          )
write_xlsx(mail_merge_wide, 
           file.path("Mail merge", outputname6)
)
