# Wave 4 ####
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               expss,
               labelled,
               collapse)
getwd()
options(scipen = 999)
rm(list = ls())
# Explore intercropping ####
# Explore field data to understand intercropping and identify intercrops 
field <- read_dta("data/eth/ess4/sect3_pp_w4.dta") %>% 
  select("household_id", "parcel_id", "field_id", "saq08", "saq09", "s3q02a", "s3q02b", "s3q2b_os",
         "s3q03", "s3q03_os", "s3q03b", "s3q04", "s3q05", "s3q07", "s3q08") %>% 
  rename(mixedcrop=s3q04)
look_for(field)
table(field$mixedcrop) # 2692 mixed
cropdata <- read_dta("data/eth/ess4/sect4_pp_w4.dta") %>% 
  select("household_id", "parcel_id", "field_id","crop_id", "s4q01b", "s4q02", "s4q03") %>% 
  rename(mixedcrop=s4q02)
look_for(cropdata)
table(cropdata$mixedcrop) #6085 mixed crops
count(cropdata, s4q01b)%>% print(n=88)
# coffee 1016 chat 832
# [6. SORGHUM]            1495
# [7. TEFF]               1223
# [8. WHEAT]               770
# [1. BARLEY]              691
# [2. MAIZE]              2023
# [3. MILLET]              263
cropdata <- cropdata %>% 
  dplyr::group_by(household_id, parcel_id, field_id) %>% 
  mutate(legume = ifelse(s4q01b %in% c(9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 24), 1, 0),
         legume = ifelse(any(mixedcrop == 2) & any(legume == 1), 1, 0)) %>% 
  ungroup()

cropdata %>% 
  filter(s4q01b %in% c(6, 7, 8, 1, 2, 3, 72, 71)) %>% 
  count(s4q01b, mixedcrop)
# s4q01b          mixedcrop             n
# <dbl+lbl>       <dbl+lbl>         <int>
#  1 [1. BARLEY]  1 [1. Pure Stand]   626
#  1 [1. BARLEY]  2 [2. Mixed]         65
#  2 [2. MAIZE]   1 [1. Pure Stand]  1311
#  2 [2. MAIZE]   2 [2. Mixed]        712
#  3 [3. MILLET]  1 [1. Pure Stand]   244
#  3 [3. MILLET]  2 [2. Mixed]         19
#  6 [6. SORGHUM] 1 [1. Pure Stand]   871
#  6 [6. SORGHUM] 2 [2. Mixed]        624
#  7 [7. TEFF]    1 [1. Pure Stand]  1202
#  7 [7. TEFF]    2 [2. Mixed]         21
#  8 [8. WHEAT]   1 [1. Pure Stand]   710
#  8 [8. WHEAT]   2 [2. Mixed]         60
#  71 [71. CHAT]  1 [1. Pure Stand]   255
#  71 [71. CHAT]  2 [2. Mixed]        577
# 72 [72. COFFEE] 1 [1. Pure Stand]   466
# 72 [72. COFFEE] 2 [2. Mixed]        550

legumemix <- cropdata %>% 
  filter(mixedcrop==2)
count(legumemix, s4q01b, legume)%>% print(n=15)

# s4q01b                    legume     n
# <dbl+lbl>                  <dbl> <int>
# 1 [1. BARLEY]                 0    61
# 1 [1. BARLEY]                 1     4
# 2 [2. MAIZE]                  0   583
# 2 [2. MAIZE]                  1   129
# 3 [3. MILLET]                 0    19
# 4 [4. OATS]                   0     3
# 6 [6. SORGHUM]                0   446
# 6 [6. SORGHUM]                1   178
# 7 [7. TEFF]                   0    21
# 8 [8. WHEAT]                  0    54
# 8 [8. WHEAT]                  1     6


# as expected, we are more likely to observe legume intercrops in maize and sorghum 
remove(cropdata,legumemix)

## Maize fields ###
# Identify maize fields
cropdata <- read_dta("data/eth/ess4/sect4_pp_w4.dta") %>% 
  select("household_id", "parcel_id", "field_id", "s4q01b", "s4q02", "s4q03")
# Identify fields where at least one parcel has maize
fields_with_maize <- cropdata %>%
  filter(s4q01b == 2) %>%
  select(household_id, parcel_id, field_id) %>%
  unique()
# Keep all parcels in those fields
maizefield <- cropdata %>%
  semi_join(fields_with_maize, by = c("household_id", "parcel_id", "field_id"))

# There are 2963 parcels in the fields with at least one parcel with maize. 


# identify maize fields intercroped with legumes
data <- maizefield %>% 
  mutate(legume = ifelse(s4q01b %in% c(9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 24), 1, 0))
data %>% count(legume)
data %>%
  filter(legume==1) %>% 
  distinct(household_id, parcel_id, field_id, .keep_all = TRUE) %>% 
  count(legume)# Remove duplicates
# Only 132 parcels have intercropping of maize and legume
rm(list = ls())


# Maize - legume rotations ####
## Cereals are: barley, maize, sorghum, millet, wheat and teff. 
## There are few fields reported more than ones with different holders.
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
field_with_diff_s3q34 <- field_with_diff_s3q34


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
field <- read_dta("data/eth/ess4/sect3_pp_w4.dta") %>% 
  select(household_id, holder_id, parcel_id, field_id, s3q03, s3q34) %>% 
  filter(s3q03==1) %>% 
  anti_join(field_with_diff_s3q34, by = c("household_id", "parcel_id", "field_id", "s3q34")) %>% 
  distinct(household_id, parcel_id, field_id, s3q34, .keep_all = TRUE)

maizedata <- read_dta("data/eth/ess4/sect4_pp_w4.dta") %>% 
  select(household_id, parcel_id, field_id,crop_id, s4q01b, s4q02, s4q03) %>% 
  filter(s4q01b==2)
maizedata %>% 
  group_by(household_id, parcel_id, field_id) %>%
  filter(n() > 1) %>%
  ungroup() # 4 observations # maize is mixed with maize for two hhs.
maizedata <- maizedata %>% 
  distinct(household_id, parcel_id, field_id, .keep_all = TRUE) # Remove duplicates
cropdata <- left_join(maizedata, field, by=c("household_id", "parcel_id", "field_id"))
cropdata %>%
  group_by(household_id, parcel_id, field_id) %>%
  filter(n() > 1) %>%
  ungroup() # 0 duplicates 
remove(field, field_with_diff_s3q34)
look_for(cropdata)
glimpse(cropdata)
cropdata <- cropdata %>% 
  mutate(rotation=ifelse(s3q34 ==1, 1, 0))
table(cropdata$rotation)
# 0    1 
# 1457  564 maize fields had legume rotation
# Identify rotation at household level
# If household rotated at least cereal field with legumes during the past three years.
hhdata <- collap(cropdata, rotation~household_id, FUN=fmax)
table(hhdata$rotation)  # 411 adopters 852 non-adopters.
remove(maizedata)

# HP data ####
hpdata <- read_dta("data/hp/Plot_dataset.dta") %>% 
  filter(country=="Ethiopia", wave==4) 
look_for(hpdata)
table(hpdata$main_crop, hpdata$intercropped) # this again signals that maize legume intercropping is not so common. 
table(hpdata$improved) # 1034 fields with improved seeds
hpdata <- hpdata %>%
  select(hh_id_merge, hh_id_obs, plot_id_merge, plot_id_obs, parcel_id_obs, parcel_id_merge, plot_area_GPS, farm_size,
         improved,organic_fertilizer, inorganic_fertilizer, intercropped, soil_fertility_index, dist_market, plot_dist_household, 
         ag_asset_index, plot_owned, drought_shock, rain_shock,flood_shock,irrigated,  nb_seasonal_crop,  
         used_pesticides, yield_value_LCU, yield_value_USD) %>% 
  mutate(plot_id=plot_id_merge)
cropdata <- cropdata %>% 
  rename(parcel_id2=parcel_id) %>% 
  mutate(plot_id = paste(holder_id, parcel_id2, field_id, sep = "-")) # field means plot in HP
plotdata <- left_join(cropdata,hpdata, by=c("plot_id")) %>% 
  mutate(mixedcrop=ifelse(s4q02==2,1,0)) %>% 
  select(-s3q03, -s3q34, -s4q02)
remove(hpdata)




# Gender ####
# Nutrition indicators ####