##### ##### November DISPOSALS ##### ##### 
# Create crop dataframe as a subset of df_raw
Disposals_Wheat <- subset(df_raw, select=c(CPH, parish, holding,item40005, item40079, item40013:item40024, item40082))
Disposals_Wheat <- Disposals_Wheat %>%
  rename(
    Production = item40005,
    Opening_stock_Oct = item40079,
    Merchants_for_Malting = item40013,
    Merchants_for_Feed = item40014,
    Merchants_for_Milling = item40015,
    Merchants_for_Seed = item40016,
    Merchants_for_Industrial = item40017,
    Merchants_for_Other = item40018,
    Farmers_in_Scotland = item40019,
    Farmers_outwith_Scotland = item40020,
    Used_for_Seed = item40021,
    Used_for_Feed = item40022,
    Waste_Other = item40023,
    Total_Disposed = item40024,
    Closing_Stock_Oct = item40082)

# Replace "na" with Crop type
Disposals_Wheat$Crop <- rep("Wheat",len=nrow(Disposals_Wheat))

# Create crop dataframe as a subset of df_raw
Disposals_Barley <- subset(df_raw, select=c(CPH, parish, holding, item40042,item40043, item40088, item40066:item40091))
Disposals_Barley <- Disposals_Barley%>%
  #  Work row by row in the dataframe
  rowwise %>%
  # Create a single production figure by combining winter and spring crop
  mutate(Production = coalesce(item40042,0) + coalesce(item40043,0))%>%
  rename(
    Opening_stock_Oct = item40088,
    Merchants_for_Malting = item40066,
    Merchants_for_Feed = item40069,
    Merchants_for_Milling = item40072,
    Merchants_for_Seed = item40075,
    Merchants_for_Industrial = item40078,
    Merchants_for_Other = item40081,
    Farmers_in_Scotland = item40084,
    Farmers_outwith_Scotland = item40087,
    Used_for_Seed = item40090,
    Used_for_Feed = item40093,
    Waste_Other = item40096,
    Total_Disposed = item40099,
    Closing_Stock_Oct = item40091)%>%
  # Organising the dataframe, then removing winter and spring production 
  select(CPH, parish, holding, Production, everything(),-item40042, -item40043)
# Replace "na" with Crop type
Disposals_Barley$Crop <- rep("Barley",len=nrow(Disposals_Barley))

# Create crop dataframe as a subset of df_raw
Disposals_Oats <- subset(df_raw, select=c(CPH, parish, holding, item40117, item40118, item40100, item40156:item40103))
Disposals_Oats <- Disposals_Oats%>%
  #  Work row by row in the dataframe
  rowwise %>%
  # Create a single production figure by combining winter and spring crop
  mutate(Production = coalesce(item40117,0) + coalesce(item40118,0))%>%
  rename(
    Opening_stock_Oct = item40100,
    Merchants_for_Malting = item40156,
    Merchants_for_Feed = item40159,
    Merchants_for_Milling = item40162,
    Merchants_for_Seed = item40165,
    Merchants_for_Industrial = item40168,
    Merchants_for_Other = item40171,
    Farmers_in_Scotland = item40174,
    Farmers_outwith_Scotland = item40177,
    Used_for_Seed = item40180,
    Used_for_Feed = item40183,
    Waste_Other = item40186,
    Total_Disposed = item40189,
    Closing_Stock_Oct = item40103)%>%
  # Organising the dataframe, then removing winter and spring production 
  select(CPH, parish, holding, Production, everything(),-item40117, -item40118)
# Replace "na" with Crop type
Disposals_Oats$Crop <- rep("Oats",len=nrow(Disposals_Oats))

# Append crop disposals dataframes
Disposals_ALL = bind_rows(Disposals_Wheat, Disposals_Barley, Disposals_Oats)
# Append and organise region data according to the CPH
Disposals_ALL <- left_join(Disposals_ALL,Sample.ITL225, by ="CPH" )
# NUTS2 format for Region and filtering out 0 values (Filter production and set na to 0)
Disposals_ALL <- Disposals_ALL %>%
  mutate(
    Region = ITL225CD,
    Region_name = ITL225NM)%>%
  select(
    CPH, parish, holding, Region, Crop, Production, everything(),-ITL225CD)%>%
  filter(
    Production>0)%>%
  mutate_all(
    ~ifelse(is.na(.), 0, .))

## Combine production and disposals ##
joined_all1 <- joined_all %>%
  mutate(Crop = ifelse(Crop %in% c("Barley S", "Barley W"), "Barley", Crop)) %>%
  mutate(Crop = ifelse(Crop %in% c("Oats S", "Oats W"), "Oats", Crop)) %>% 
  mutate(Crop = ifelse(Crop %in% c("OSRape S", "OSRape W"), "OSRape", Crop)) %>% 
  group_by(CPH, parish, holding, Region, Crop) %>%  # Group by CPH and the crop (remove suffixes from crop names)
  summarise(
    Production = sum(Production), 
    Wholecrop = ifelse(any(Wholecrop == "YES"), "YES", "NO"), 
    .groups = "drop") %>% 
  anti_join(removals_FF_WC, by = c("parish", "holding", "Crop", "Region")) 

combined_df <- left_join(joined_all1, Disposals_ALL, by = c("CPH", "parish", "holding", "Crop", "Region","Production")) %>% 
  # Remove rows with missing key identifiers
  filter(!is.na(Opening_stock_Oct))


#  Disposals per crop type as a subset of Disposals_ALL
Disposals_Crop <- combined_df %>%
  select(Crop, Production, Merchants_for_Malting, Merchants_for_Feed, Merchants_for_Milling,
         Merchants_for_Seed, Merchants_for_Industrial, Merchants_for_Other, Farmers_in_Scotland,
         Farmers_outwith_Scotland, Used_for_Seed, Used_for_Feed, Waste_Other, Closing_Stock_Oct) %>%
  group_by(Crop) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))
head(Disposals_Crop)
 
# #export csv
# # filename appropriate for data upload to erdm
# str4 <- " - November - Disposals - Data - Raw Data - Formatted disposals data output - "
# outputname2 <- paste(
#   crop_year,
#   str4,
#   format(Sys.Date(), "%d %B"),
#   str3,
#   sep = ""
# )
# write.csv(Disposals_ALL, outputname2, row.names = FALSE)

#export csv
# filename appropriate for data upload to erdm
str5 <- " - November - Data - Raw Data - Formatted production and disposals data output - "
outputname3 <- paste(
  crop_year,
  str5,
  format(Sys.Date(), "%d %B"),
  str3,
  sep = ""
)
write.csv(combined_df, outputname3, row.names = FALSE)

