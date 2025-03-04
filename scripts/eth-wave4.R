# Wave 4 ####
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               labelled,
               collapse)
getwd()
options(scipen = 999)
rm(list = ls())

# To explore inercropping refer to eth-intercropping.qmd file in the outputs. 

# Maize - legume rotations ####
## There are few fields reported more than 1 with different holders.
## These are probably divided between hh members.
## Let us first identify these fields
## Identify groups where s3q34 has different values
field_with_diff_s3q34 <- read_dta("data/eth/ess4/sect3_pp_w4.dta") %>% 
  filter(s3q03==1) %>% 
  group_by(household_id, parcel_id, field_id) %>%  # Group by household, parcel, and field
  mutate(unique_s3q34 = n_distinct(s3q34)) %>%  # Count unique values of s3q34 in each group
  filter(unique_s3q34 > 1) %>%  # Keep only groups where s3q34 has more than one unique value
  ungroup()
field_with_diff_s3q34 %>% count(unique_s3q34) # 12 observations 


# At this moment this data is to identify if rotation was done in the field.
# just check the duplicates for rotation. Drop duplicates with no informative information. 
field_with_diff_s3q34 %>% 
  select(household_id, parcel_id, field_id, s3q34 ) %>% 
  head(.,12)

# obs to be dropped: 
# household_id       parcel_id field_id s3q34 
# 010103088800709032         1        2 2
# 030606088800202059         1        1 2
# 040606088801505048         2        1 2
# 060103088806701051         4        1 2
# 060206088800203045         1        1 2
# and one with pasture

field_with_diff_s3q34 <- field_with_diff_s3q34 %>% 
  filter(s3q34==2) %>%
  select(household_id, parcel_id, field_id, s3q34) %>%
  distinct()
head(field_with_diff_s3q34)

# drop observation above from the field data  
field <- read_dta("data/eth/ess4/sect3_pp_w4.dta") %>% #field roster
  select(household_id, holder_id, parcel_id, field_id, s3q03, s3q34) %>% 
  filter(s3q03==1) %>% 
  anti_join(field_with_diff_s3q34, by = c("household_id", "parcel_id", "field_id", "s3q34")) %>% 
  distinct(household_id, parcel_id, field_id, s3q34, .keep_all = TRUE)
remove(field_with_diff_s3q34)
maizedata <- read_dta("data/eth/ess4/sect4_pp_w4.dta") %>%  #crop roster
  select(household_id, parcel_id, field_id,crop_id, s4q01b, s4q02, s4q03) %>% 
  filter(s4q01b==2)
maizedata %>% 
  group_by(household_id, parcel_id, field_id) %>%
  filter(n() > 1) %>%
  ungroup() # 4 observations # maize is mixed with maize for two hhs.
maizedata <- maizedata %>% 
  distinct(household_id, parcel_id, field_id, .keep_all = TRUE) # Remove duplicates to identify maize grown fields
cropdata <- left_join(maizedata, field, by=c("household_id", "parcel_id", "field_id")) # maize fields with field data 
cropdata %>%
  group_by(household_id, parcel_id, field_id) %>%
  filter(n() > 1) %>%
  ungroup() # 0 duplicates 
remove(field)
look_for(cropdata)
glimpse(cropdata)
cropdata4 <- cropdata %>% 
  mutate(rotation=ifelse(s3q34 ==1, 1, 0))
table(cropdata4$rotation)
# 0    1 
# 1457  564 maize fields had legume rotation
# Identify rotation at household level
# If household rotated at least one maize field with legumes during the past three years.
rotation4 <- collap(cropdata4, rotation~household_id, FUN=fmax)
table(rotation4$rotation)  # 411 adopters 852 non-adopters.
remove(maizedata, cropdata)

# HP data ####
hpdata <- read_dta("data/hp/Plot_dataset.dta") %>% 
  filter(country=="Ethiopia", wave==4) 
look_for(hpdata)
hpdata %>%
  group_by(hh_id_obs, parcel_id_obs, plot_id_obs) %>%
  filter(n() > 1) %>%
  ungroup() # 0 duplicates - plots are uniquely identified  
table(hpdata$main_crop, hpdata$intercropped) # 1239  268 intercropped maize 
table(hpdata$improved) # 1034 fields with improved seeds
hpdata <- hpdata %>%
  select(hh_id_merge, hh_id_obs, plot_id_merge, plot_id_obs, parcel_id_obs, parcel_id_merge, plot_area_GPS, main_crop, farm_size,
         manager_id_merge, manager_id_obs, age_manager, female_manager, formal_education_manager, primary_education_manager, respondent_id_obs, 
         respondent_id_merge, improved,organic_fertilizer, inorganic_fertilizer, intercropped, soil_fertility_index, dist_market, 
         plot_dist_household, ag_asset_index, plot_owned, plot_certificate, drought_shock, rain_shock,flood_shock,irrigated,  nb_seasonal_crop,  
         used_pesticides, yield_value_LCU, yield_value_USD) %>% 
  mutate(plot_id=plot_id_merge)
cropdata4 <- cropdata4 %>% 
  rename(parcel_id2=parcel_id) %>% 
  mutate(plot_id = paste(holder_id, parcel_id2, field_id, sep = "-")) # field means plot in HP
plotdata4 <- left_join(cropdata4,hpdata, by=c("plot_id")) %>% 
  select(-s3q03, -s3q34, -s4q02)
remove(hpdata)
# generate a variable for duplicates of household_id in plotdata 
plotdata4 <- plotdata4 %>% 
  group_by(household_id) %>%
  mutate( dupl=n()) %>%
  ungroup()
table(plotdata4$dupl)
# 1   2   3   4   5   6   8 
# 767 638 333 208  55  12   8 
plotdata4 %<>% select(-s4q01b, -dupl)
table(plotdata4$main_crop)
# BARLEY    BEANS AND OTHER LEGUMES                   MAIZE                   OTHER 
# 6                         35                    1503                      46 
# PERENNIAL/FRUIT                 SORGHUM     TUBERS / ROOT CROPS 
# 207                          109                      18 

# From the above table we see that, main crop in some of the maize cultivated plots are different crops like beans and legumes or fruti trees.
# There are many `other` crops which needs to be closely investigated in the dataset what these other crops are.
plotdata4 %<>% mutate(main_legume=if_else(main_crop=="BEANS AND OTHER LEGUMES", 1,0)) 
var_label(plotdata4$main_legume) <- "legume is main crop in this plot"
look_for(plotdata4)
remove(cropdata4)
# Gender ####

# Nutrition indicators ####
# HDDS
# total consumption; LATER consider alternative: Nominal annual expenditure on food per adult equivalent
# consumption quantile
hhhp4 <- read_dta("data/hp/Household_dataset.dta") %>% 
  filter(country=="Ethiopia", wave==4) %>% 
  select(hh_id_merge, hh_id_obs, hh_size, hh_shock, hh_dependency_ratio,totcons_USD, cons_quint, hh_asset_index, HDDS, share_kg_sold)
look_for(hhhp4)
