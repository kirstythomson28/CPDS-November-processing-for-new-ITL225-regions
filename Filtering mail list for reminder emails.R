##Code to filter mail list for reminders - run utility script first

long_full_data <- pivot_longer(
  full_data,
  cols = starts_with("location"),
  names_to = "location",
  values_to = "CPH"
)%>%
  drop_na(CPH) %>%  # Removes rows where 'score' is NA
  separate(CPH, into = c("parish", "holding"), sep = "/", convert = TRUE, remove = FALSE)


# Remove rows in df_raw from full_dataset based on the 'parish' and holding numbers
not_submitted_yet <- anti_join(long_full_data, df_raw, by = c("parish", "holding"))


wide_not_submitted_yet <- not_submitted_yet %>%
  select(-c(parish,holding))%>%
  pivot_wider(
    names_from = location,
    values_from = CPH
  )

## Remove list of manual records to be removed following contact from the farmers via email 
wide_not_submitted_yet <- anti_join(wide_not_submitted_yet, manual_removals, by = "brn")


#export xlsx
str1 <- "mail_merge_not_submitted_yet"
str3 <- ".xlsx"
outputname <- paste(str1, format(Sys.Date(), format="%d %b"), str3) 
write_xlsx(wide_not_submitted_yet,
           file.path("Mail merge", outputname))