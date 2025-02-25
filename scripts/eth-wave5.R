# Wave 5 ####
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               expss,
               labelled,
               collapse)
getwd()
options(scipen = 999)

# To explore inercropping refer to eth-intercropping.qmd file in the outputs. 

# Maize - legume rotations ####
## There are few fields reported more than 1 with different holders.
## These are probably divided between hh members.
## Let us first identify these fields
## Identify groups where s3q34 has different values
field_with_diff_s3q34 <- read_dta("data/eth/ess5/sect3_pp_w5.dta") %>% 
  filter(s3q03==1) %>% 
  group_by(household_id, parcel_id, field_id) %>%  # Group by household, parcel, and field
  mutate(unique_s3q34 = n_distinct(s3q34)) %>%  # Count unique values of s3q34 in each group
  filter(unique_s3q34 > 1) %>%  # Keep only groups where s3q34 has more than one unique value
  ungroup()
field_with_diff_s3q34 %>% count(unique_s3q34) # 8 observations 


# At this moment this data is to identify if rotation was done in the field.
# just check the duplicates for rotation. Drop duplicates with no informative information. 
field_with_diff_s3q34 %>% 
  select(household_id, parcel_id, field_id, s3q34 ) %>% 
  head(.,8)

# # A tibble: 8 × 4
# household_id       parcel_id field_id s3q34     
# <chr>                  <dbl>    <dbl> <dbl+lbl> 
# 1 060103088806701051         3        1 2 [2. NO] 
# 2 060103088806701051         3        1 1 [1. YES]
# 3 060107088800402060         1        6 1 [1. YES]
# 4 060107088800402060         2        1 1 [1. YES]
# 5 060107088800402060         3        1 1 [1. YES]
# 6 060107088800402060         1        6 2 [2. NO] 
# 7 060107088800402060         2        1 2 [2. NO] 
# 8 060107088800402060         3        1 2 [2. NO]

field_with_diff_s3q34 <- field_with_diff_s3q34 %>% 
  filter(s3q34==2) %>%
  select(household_id, parcel_id, field_id, s3q34) %>%
  distinct()
head(field_with_diff_s3q34)

# drop observation above from the field data  
field <- read_dta("data/eth/ess5/sect3_pp_w5.dta") %>% #field roster
  select(household_id, holder_id, parcel_id, field_id, s3q03, s3q34) %>% 
  filter(s3q03==1) %>% 
  anti_join(field_with_diff_s3q34, by = c("household_id", "parcel_id", "field_id", "s3q34")) %>% 
  distinct(household_id, parcel_id, field_id, s3q34, .keep_all = TRUE)
remove(field_with_diff_s3q34)
maizedata <- read_dta("data/eth/ess5/sect4_pp_w5.dta") %>%  #crop roster
  select(household_id, parcel_id, field_id,crop_id, s4q01b, s4q02, s4q03) %>% 
  filter(s4q01b==2)
maizedata %>% 
  group_by(household_id, parcel_id, field_id) %>%
  filter(n() > 1) %>%
  ungroup() # 10 observations # maize is mixed with maize for two hhs.
maizedata <- maizedata %>% 
  distinct(household_id, parcel_id, field_id, .keep_all = TRUE) # Remove duplicates
cropdata <- left_join(maizedata, field, by=c("household_id", "parcel_id", "field_id")) # maize fields with field data 
cropdata %>%
  group_by(household_id, parcel_id, field_id) %>%
  filter(n() > 1) %>%
  ungroup() # 0 duplicates 
remove(field)
look_for(cropdata)
glimpse(cropdata)
cropdata5 <- cropdata %>% 
  mutate(rotation=ifelse(s3q34 ==1, 1, 0))
table(cropdata5$rotation)
# 0    1 
# 1305  633 maize fields had legume rotation
# Identify rotation at household level
# If household rotated at least one maize field with legumes during the past three years.
hhdata5 <- collap(cropdata5, rotation~household_id, FUN=fmax)
table(hhdata5$rotation)  # 392 adopters 612 non-adopters.
remove(maizedata, cropdata)

# HP data ####
hpdata <- read_dta("data/hp/Plot_dataset.dta") %>% 
  filter(country=="Ethiopia", wave==5) 
look_for(hpdata)
table(hpdata$main_crop, hpdata$intercropped) # 1048  431 intercropped maize 
table(hpdata$improved) # 1049 fields with improved seeds
hpdata <- hpdata %>%
  select(hh_id_merge, hh_id_obs, plot_id_merge, plot_id_obs, parcel_id_obs, parcel_id_merge, plot_area_GPS, farm_size,
         improved,organic_fertilizer, inorganic_fertilizer, intercropped, soil_fertility_index, dist_market, plot_dist_household, 
         ag_asset_index, plot_owned, drought_shock, rain_shock,flood_shock,irrigated,  nb_seasonal_crop,  
         used_pesticides, yield_value_LCU, yield_value_USD) %>% 
  mutate(plot_id=plot_id_merge)
cropdata5 <- cropdata5 %>% 
  rename(parcel_id2=parcel_id) %>% 
  mutate(plot_id = paste(holder_id, parcel_id2, field_id, sep = "-")) # field means plot in HP
plotdata5 <- left_join(cropdata5,hpdata, by=c("plot_id")) %>% 
  mutate(mixedcrop=ifelse(s4q02==2,1,0)) %>% 
  select(-s3q03, -s3q34, -s4q02)
remove(hpdata)
# generate a variable for duplicates of household_id in plotdata 
plotdata5 <- plotdata5 %>% 
  group_by(household_id) %>%
  mutate( dupl=n()) %>%
  ungroup() # 0 duplicates 
table(plotdata5$dupl)
