####################################################################
#combine parish/holding
df_raw$CPH <- paste(df_raw$parish, df_raw$holding, sep="/")

#move CPH to 1st column
df_raw %>% relocate(CPH, .before = parish)

####subset and alter wheat data ####

#extract and subset just the required wheat data
Wheat<-subset(df_raw, select=c(CPH, parish, holding, prefill40002:item40009))

#rename items
Wheat<-Wheat %>% 
  rename(
    Area = prefill40002,
    Production = item40005,
    Yield = item40450,
    Moisture_content = item40008,
    Whole_cropped = item40009)

#Fill Crop column with "Wheat" check n obs! (here 17obs)
Wheat$Crop <- rep("Wheat",len=nrow(Wheat))

####subset for Barely_S #####
#extract and subset just the required spring barley data
Barley_S <- subset(df_raw, select=c(CPH, parish, holding, item40044,item40043,item40452, item40052, item40058 ))

#rename items
Barley_S<-Barley_S %>% 
  rename(
    Area = item40044,
    Production = item40043,
    Yield = item40452,
    Moisture_content = item40052,
    Whole_cropped = item40058)

#Fill Crop column with "Barley S" check n obs

Barley_S$Crop <- rep("Barley S",len=nrow(Barley_S))


####subset for Barley_W #####
#extract and subset just the required W Barley data

Barley_W <- subset(df_raw, select=c(CPH, parish, holding, item40040, item40042, item40451, item40051, item40057 ))


#rename items
Barley_W<-Barley_W %>% 
  rename(
    Area = item40040,
    Production = item40042,
    Yield = item40451,
    Moisture_content = item40051,
    Whole_cropped = item40057)

#Fill Crop column with "Barley" check n obs (here 17obs)

Barley_W$Crop <- rep("Barley W",len=nrow(Barley_W))

####subset for Oats_W #####
#extract and subset just the required Oats data

Oats_W <- subset(df_raw, select=c(CPH, parish, holding, item40047,item40117, item40453, item40123, item40126))


#rename items
Oats_W <- Oats_W %>% 
  rename(
    Area = item40047,
    Production = item40117,
    Yield = item40453,
    Moisture_content = item40123,
    Whole_cropped = item40126)

#Fill Crop column with "Oats_W" check n obs (here 17obs)

Oats_W$Crop <- rep("Oats W",len=nrow(Oats_W))

####subset for Oats_S #####
#extract and subset just the required Oats data

Oats_S <- subset(df_raw, select=c(CPH, parish, holding, item40048,item40118, item40454, item40124, item40127))


#rename items
Oats_S<-Oats_S %>% 
  rename(
    Area = item40048,
    Production = item40118,
    Yield = item40454,
    Moisture_content = item40124,
    Whole_cropped = item40127)

#Fill Crop column with "Oats_S" check n obs (here 17obs)

Oats_S$Crop <- rep("Oats S",len=nrow(Oats_S))

####subset for OSR_S #####
#extract and subset just the required Oats data

OSR_S <- subset(df_raw, select=c(CPH, parish, holding, item40059,item40414, item40456, item40420, item40423))


#rename items
OSR_S<-OSR_S %>% 
  rename(
    Area = item40059,
    Production = item40414,
    Yield = item40456,
    Moisture_content = item40420,
    Whole_cropped = item40423)

#Fill Crop column with "OSR_S" check n obs (here 17obs)

OSR_S$Crop <- rep("OSRape S",len=nrow(OSR_S))


####subset for OSR_W #####
#extract and subset just the required Oats data

OSR_W<-subset(df_raw, select=c(CPH, parish, holding, item40055,item40413, item40455, item40419, item40422))


#rename items
OSR_W<-OSR_W %>% 
  rename(
    Area = item40055,
    Production = item40413,
    Yield = item40455,
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
      select(parish, holding, Crop, Region, Wholecrop, reason, `Final decision`))

joined_all <- joined_all %>%
  anti_join(removals_FF_WC, by = c("parish", "holding", "Crop", "Region"))    
        
  
#export xlsx
#filename appropriate for data upload to erdm
str2 <- " - November - Production - Data - Raw Data - Formatted production data output (FF & WhCr removed) - "
outputname2 <- paste(
  crop_year,
  str2,
  format(Sys.Date(), "%d %B"),
  str7,
  sep = ""
)
write_xlsx(joined_all,
           file.path("November results", outputname2))

