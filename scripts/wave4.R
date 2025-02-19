# Wave 4 ####
## necessary libraries
pacman::p_load(tidyverse,
               dplyr,
               haven,
               tibble,
               expss,
               labelled,
               collapse)
getwd()
options(scipen = 999)
rm(list = ls())
## Explore intercropping ####
# Explore field data to understand intercropping and identify intercrops 
field <- read_dta("data/eth/ess3/sect3_pp_w4.dta") %>% 
  select("household_id", "parcel_id", "field_id", "saq08", "saq09", "s3q02a", "s3q02b", "s3q2b_os",
         "s3q03", "s3q03_os", "s3q03b", "s3q04", "s3q05", "s3q07", "s3q08") %>% 
  rename(mixedcrop=s3q04)
look_for(field)
table(field$mixedcrop) # 2692 mixed
cropdata <- read_dta("data/eth/ess3/sect4_pp_w4.dta") %>% 
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

### Maize fields ####
# Identify maize fields
maizefield <- read_dta("data/eth/ess3/sect4_pp_w4.dta") %>% 
  select("household_id", "parcel_id", "field_id","s4q01b") %>% 
  filter(s4q01b==2) %>% 
  select(-s4q01b) %>% 
  distinct(household_id, parcel_id, field_id, .keep_all = TRUE) %>%   # Remove duplicates
  mutate(maizefil=1)
cropdata <- read_dta("data/eth/ess3/sect4_pp_w4.dta") %>% 
  select("household_id", "parcel_id", "field_id", "s4q01b", "s4q02", "s4q03")
# identify all maize planted fields
data <- left_join(cropdata, maizefield, by=c("household_id", "parcel_id", "field_id")) %>% 
  filter(maizefil==1)
data %>% count(s4q02) %>% print(n=25) #1592
data %>% filter(s4q02==2) %>% count(s4q01b) %>% print(n=25) #712 maize
# identify maize fields intercroped with legumes
data <- data %>% 
  mutate(legume = ifelse(s4q01b %in% c(9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 24), 1, 0))
data %>% count(legume)
intercrop <- data %>%
  filter(legume==1) %>% 
  distinct(household_id, parcel_id, field_id, .keep_all = TRUE) # Remove duplicates
# Only 132 parcels have intercropping of maize and legume


## Explore cereal legume rotations. 
field <- read_dta("data/eth/ess3/sect3_pp_w4.dta") %>% 
  #select(household_id, parcel_id, field_id, s3q03, s3q34) %>% 
  filter(s3q03==1)
# Identify groups where s3q34 has different values
field_with_diff_s3q34 <- field %>% 
  group_by(household_id, parcel_id, field_id) %>%  # Group by household, parcel, and field
  mutate(unique_s3q34 = n_distinct(s3q34)) %>%  # Count unique values of s3q34 in each group
  filter(unique_s3q34 > 1) %>%  # Keep only groups where s3q34 has more than one unique value
  ungroup()  # Ungroup the dataa
field_with_diff_s3q34 %>% count(unique_s3q34)
# some fields have different holders, probably divided between hh members. HOW TO ADDRESS THIS WHEN IDENTIFYING IF ROTATION WAS DONE? 
# MAYBE TAKE THE SMALLES VALUE OF 34 1 IS YES 2 IS NO 
#  
field <- read_dta("data/eth/ess3/sect3_pp_w4.dta") %>% 
  select(household_id, parcel_id, field_id, s3q03, s3q34) %>% 
  filter(s3q03==1) %>% 
  distinct(household_id, parcel_id, field_id, s3q34, .keep_all = TRUE) %>% 
  distinct(household_id, parcel_id, field_id, .keep_all = TRUE)# Remove duplicates

cropdata <- read_dta("data/eth/ess3/sect4_pp_w4.dta") %>% 
  select("household_id", "parcel_id", "field_id","crop_id", "s4q01b", "s4q02", "s4q03") 
cropdata %>%   count(s4q01b)
# Cereals are: barley, maize, sorghum, millet, wheat and teff
cropdata <- cropdata %>% 
  filter(s4q01b %in% c(1,2,3,6,7,8)) %>% 
  distinct(household_id, parcel_id, field_id, .keep_all = TRUE) # Remove duplicates
cropdata <- left_join(cropdata, field, by=c("household_id", "parcel_id", "field_id"))
