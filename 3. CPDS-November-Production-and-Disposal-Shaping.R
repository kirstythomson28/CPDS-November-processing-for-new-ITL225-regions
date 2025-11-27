####################################################################
#combine parish/holding
df_raw$CPH <- paste(df_raw$parish, df_raw$holding, sep="/")

#move CPH to 1st column
df_raw %>% relocate(CPH, .before = parish)

####subset and alter wheat data ####

#extract and subset just the required wheat data
Wheat<-subset(df_raw, select=c(CPH, parish, holding, prefill40002:item40009, wheat_correctedyield, wheat_correctedproduction))

#rename items
Wheat<-Wheat %>% 
  rename(
    Area = prefill40002,
    Production = item40005,
    Yield = item40450,
    adj_yield = wheat_correctedyield,
    adj_production = wheat_correctedproduction,
    Moisture_content = item40008,
    Whole_cropped = item40009)

#Fill Crop column with "Wheat" check n obs! (here 17obs)
Wheat$Crop <- rep("Wheat",len=nrow(Wheat))

####subset for Barely_S #####
#extract and subset just the required spring barley data
Barley_S <- subset(df_raw, select=c(CPH, parish, holding, item40044,item40043,
                                    item40452, item40052, item40058, barleyspring_correctedyield,
                                    barleyspring_correctedproduction))

#rename items
Barley_S<-Barley_S %>% 
  rename(
    Area = item40044,
    Production = item40043,
    Yield = item40452,
    adj_yield = barleyspring_correctedyield,
    adj_production = barleyspring_correctedproduction,
    Moisture_content = item40052,
    Whole_cropped = item40058)

#Fill Crop column with "Barley S" check n obs

Barley_S$Crop <- rep("Barley S",len=nrow(Barley_S))


####subset for Barley_W #####
#extract and subset just the required W Barley data

Barley_W <- subset(df_raw, select=c(CPH, parish, holding, item40040, item40042, 
                                    item40451, item40051, item40057,
                                    barleywinter_correctedyield, barleywinter_correctedproduction))


#rename items
Barley_W<-Barley_W %>% 
  rename(
    Area = item40040,
    Production = item40042,
    Yield = item40451,
    adj_yield = barleywinter_correctedyield,
    adj_production = barleywinter_correctedproduction,
    Moisture_content = item40051,
    Whole_cropped = item40057)

#Fill Crop column with "Barley" check n obs (here 17obs)

Barley_W$Crop <- rep("Barley W",len=nrow(Barley_W))

####subset for Oats_W #####
#extract and subset just the required Oats data

Oats_W <- subset(df_raw, select=c(CPH, parish, holding, item40047,item40117, item40453, 
                                  item40123, item40126, oatswinter_correctedyield, 
                                  oatswinter_correctedproduction))


#rename items
Oats_W <- Oats_W %>% 
  rename(
    Area = item40047,
    Production = item40117,
    Yield = item40453,
    adj_yield = oatswinter_correctedyield,
    adj_production = oatswinter_correctedproduction,
    Moisture_content = item40123,
    Whole_cropped = item40126)

#Fill Crop column with "Oats_W" check n obs (here 17obs)

Oats_W$Crop <- rep("Oats W",len=nrow(Oats_W))

####subset for Oats_S #####
#extract and subset just the required Oats data

Oats_S <- subset(df_raw, select=c(CPH, parish, holding, item40048,item40118, item40454,
                                  item40124, item40127, oatsspring_correctedyield,oatsspring_correctedproduction))


#rename items
Oats_S<-Oats_S %>% 
  rename(
    Area = item40048,
    Production = item40118,
    Yield = item40454,
    adj_yield = oatsspring_correctedyield,
    adj_production = oatsspring_correctedproduction,
    Moisture_content = item40124,
    Whole_cropped = item40127)

#Fill Crop column with "Oats_S" check n obs (here 17obs)

Oats_S$Crop <- rep("Oats S",len=nrow(Oats_S))

####subset for OSR_S #####
#extract and subset just the required Oats data

OSR_S <- subset(df_raw, select=c(CPH, parish, holding, item40059,item40414, item40456,
                                 item40420, item40423, oilseedrapespring_correctedyield,
                                 oilseedrapespring_correctedproduction))


#rename items
OSR_S<-OSR_S %>% 
  rename(
    Area = item40059,
    Production = item40414,
    Yield = item40456,
    adj_yield = oilseedrapespring_correctedyield,
    adj_production = oilseedrapespring_correctedproduction,
    Moisture_content = item40420,
    Whole_cropped = item40423)

#Fill Crop column with "OSR_S" check n obs (here 17obs)

OSR_S$Crop <- rep("OSRape S",len=nrow(OSR_S))


####subset for OSR_W #####
#extract and subset just the required Oats data

OSR_W<-subset(df_raw, select=c(CPH, parish, holding, item40055,item40413, item40455,
                               item40419, item40422, oilseedrapewinter_correctedyield,
                               oilseedrapewinter_correctedproduction))


#rename items
OSR_W<-OSR_W %>% 
  rename(
    Area = item40055,
    Production = item40413,
    Yield = item40455,
    adj_yield = oilseedrapewinter_correctedyield,
    adj_production = oilseedrapewinter_correctedproduction,
    Moisture_content = item40419,
    Whole_cropped = item40422)


#Fill Crop column with "OSR_W" check n obs

OSR_W$Crop <- rep("OSRape W",len=nrow(OSR_W))


#append all the separate crop dfs
Crops_all = bind_rows(Wheat, Barley_S, Barley_W, Oats_S, Oats_W, OSR_S,OSR_W)

#combine parish/holding
Sample$CPH <- paste(Sample$parish, Sample$holding, sep="/")

#move CPH to 1st column
Sample_2<- Sample %>% relocate(CPH)

#Use Sample2 (created from sample with CPH formed) to extract NUTS2 and CPH
Sample.ITL225<-subset(Sample_2, select=c(CPH, ITL225CD,ITL225NM))

#Join the data frames
joined_all <- left_join(Crops_all,Sample.ITL225, by ="CPH" )%>%
  mutate(Wholecrop = case_when(Whole_cropped == 1 ~ 'YES',
                               Whole_cropped == 2 ~ 'NO'))%>%
  mutate(Region = ITL225CD)%>% 
  relocate(CPH, parish, holding, Region, Crop, Area, Production, Moisture_content, #move order
           Wholecrop, Whole_cropped) %>% 
  filter(Area>0) %>% #remove records generated with no area
  select(-Whole_cropped)

wholecropped <- joined_all %>%
  filter(Wholecrop == "YES") %>%
  mutate(reason = "wholecropped",
         `Final decision` = "remove")


removals_FF_WC <- removals_FF %>%
  bind_rows(
    wholecropped %>%
      mutate(Wholecrop = as.character(Wholecrop)) %>%
      select(CPH, parish, holding, Crop, Region, Yield, adj_yield, reason, Wholecrop, `Final decision`))

joined_all <- joined_all %>%
  anti_join(removals_FF_WC, by = c("parish", "holding", "Crop", "Region"))    
        
  
#export xlsx
#filename appropriate for data upload to erdm
str2 <- " - Formatted Nov production data output (FF & WhCr removed) - "
outputname2 <- paste(
  crop_year,
  str2,
  format(Sys.Date(), "%d %B"),
  str7,
  sep = ""
)
write_xlsx(joined_all,
           file.path("November results", outputname2))

################################################################################
####################### November DISPOSALS #####################################
################################################################################

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

#export csv
# filename appropriate for data upload to erdm
str4 <- " - Formatted Nov disposals data output - "
outputname2 <- paste(
  crop_year,
  str4,
  format(Sys.Date(), "%d %B"),
  str3,
  sep = ""
)

# Save to November results folder
write.csv(
  Disposals_ALL,
  file = file.path("November results", outputname2),
  row.names = FALSE
)

#export csv
# filename appropriate for data upload to erdm
str5 <- " - Formatted Nov production and disposals data - "
outputname3 <- paste(
  crop_year,
  str5,
  format(Sys.Date(), "%d %B"),
  str3,
  sep = ""
)
write.csv(
  combined_df,
  file = file.path("November results", outputname3), 
  row.names = FALSE)
