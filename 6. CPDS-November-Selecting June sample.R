####### Selecting Sample for July ########
data <- combined_df

# Create a df of any entries that were wholecropped 
whole_cropped <- joined_all %>%
  filter(Wholecrop == "YES")%>% # Rows from survey where whole cropped
  select(CPH) # Keep only CPH

removals_cph <- removals %>% 
  distinct(CPH)

removed_CPH <- whole_cropped %>% 
  bind_rows(removals_cph)%>% 
  distinct(CPH, .keep_all = TRUE)

data <- data %>%
  filter(!CPH %in% removed_CPH$CPH)%>% #Remove any row if CPH is in removed cph
  filter(Crop != "OSRape") #Remove OSRape

#Remove any where no crop is left
data <- data %>%
  filter(Opening_stock_Oct != "0")%>%
  filter(Closing_Stock_Oct != "0")

July_Sample <- data %>%  
  select(CPH, parish, holding) %>%      
  distinct(CPH, .keep_all = TRUE) 

### Mailmerge ###

mail_merge <- Sample_2 %>%
  filter(CPH %in% July_Sample$CPH)%>%
  select(CPH, parish, holding, brn, primary_email)

July_Sample <- July_Sample %>%
  select(-CPH)

colnames(July_Sample) <- NULL

# Filename appropriate for data upload to ERDM
str6 <- " - November - Data - Formatted data output for july sample - location only - "
outputname4 <- paste(
  crop_year,
  str6,
  format(Sys.Date(), "%d %B"),
  str7,
  sep = ""
)
write_xlsx(July_Sample, outputname4)

# Filename appropriate for data upload to ERDM
str8 <- " - November - Data - CPH removed from july sample - location only - "
outputname5 <- paste(
  crop_year,
  str8,
  format(Sys.Date(), "%d %B"),
  str7,
  sep = ""
)
write_xlsx(removed_CPH, outputname5)

# Filename appropriate for data upload to ERDM
str9 <- " - November - Data - july sample - mail merge - "
outputname6 <- paste(
  crop_year,
  str9,
  format(Sys.Date(), "%d %B"),
  str7,
  sep = ""
)
write_xlsx(mail_merge, outputname6)
