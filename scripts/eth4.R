# eth4 ----
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               labelled,
               collapse)
getwd()
options(scipen = 999)
rm(list = ls())
### ### ### ### ### ### ### ###
# HP data ----
### ### ### ### ### ### ### ###
hpdata <- read_dta("data/hp/Plot_dataset.dta") |>
  filter(country=="Ethiopia", wave==4) 
# We concentrate on the plots where maize is the main crop. 
maizeplots <- hpdata |> 
  filter(main_crop=="MAIZE") |> 
  select(hh_id_merge, parcel_id_merge, plot_id_merge) |> 
  mutate(plot_id=plot_id_merge)
maizehh <- maizeplots |>  distinct(hh_id_merge) # 1025 households
hpdata|>
  filter(main_crop=="MAIZE") |> 
  group_by(hh_id_obs, parcel_id_obs, plot_id_obs)|>
  filter(n() > 1)|>
  ungroup() # 0 duplicates - plots are uniquely identified  
hpdata <- hpdata|>
  select(hh_id_merge, hh_id_obs, plot_id_merge, plot_id_obs, parcel_id_obs, parcel_id_merge, plot_area_GPS, main_crop, farm_size,
         manager_id_merge, manager_id_obs, age_manager, female_manager, formal_education_manager, primary_education_manager, respondent_id_obs, 
         respondent_id_merge, improved,organic_fertilizer, inorganic_fertilizer, intercropped, soil_fertility_index, dist_market, 
         plot_dist_household, ag_asset_index, plot_certificate, drought_shock, rain_shock,flood_shock,irrigated,  nb_seasonal_crop,  
         used_pesticides, yield_value_LCU, yield_value_USD) |>
  mutate(plot_id=plot_id_merge)
look_for(hpdata) #many missing

### ### ### ### ### ### ### ###
# Maize - legume intercropping----
# RARE! Please refer to the qmd file \leg4dev\outputs\ethiopia.qmd

### ### ### ### ### ### ### ###
# Maize - legume rotation----
### ### ### ### ### ### ### ###
## There are few fields reported more than 1 with different holders.
## These are probably divided between hh members.
## Let us first identify these fields
## Identify groups where s3q34 has different values
field_with_diff_s3q34 <- read_dta("data/eth/ess4/sect3_pp_w4.dta") |>
  filter(s3q03==1) |>
  group_by(household_id, parcel_id, field_id) |> # Group by household, parcel, and field
  mutate(unique_s3q34 = n_distinct(s3q34)) |> # Count unique values of s3q34 in each group
  filter(unique_s3q34 > 1) |> # Keep only groups where s3q34 has more than one unique value
  ungroup()
field_with_diff_s3q34 |>count(unique_s3q34) # 12 observations 

# At this moment this data is to identify if rotation was done in the field.
# just check the duplicates for rotation. Drop duplicates with no informative information. 
field_with_diff_s3q34 |>
  select(household_id, parcel_id, field_id, s3q34 ) |>
  head(12)

# obs to be dropped: 
# household_id       parcel_id field_id s3q34 
# 010103088800709032         1        2 2
# 030606088800202059         1        1 2
# 040606088801505048         2        1 2
# 060103088806701051         4        1 2
# 060206088800203045         1        1 2
# and one with pasture

field_with_diff_s3q34 <- field_with_diff_s3q34 |>
  filter(s3q34==2)|>
  select(household_id, parcel_id, field_id, s3q34)|>
  distinct()
head(field_with_diff_s3q34)

# drop observation above from the field data  
field <- read_dta("data/eth/ess4/sect3_pp_w4.dta") |>#field roster
  select(household_id, holder_id, parcel_id, field_id, s3q03, s3q13, s3q34) |>
  filter(s3q03==1) |>
  anti_join(field_with_diff_s3q34, by = c("household_id", "parcel_id", "field_id", "s3q34")) |>
  distinct(household_id, parcel_id, field_id, s3q34, .keep_all = TRUE)
remove(field_with_diff_s3q34)
field <- field |> 
  mutate(plot_id = paste(holder_id, parcel_id, field_id, sep = "-"))
maizedata <- read_dta("data/eth/ess4/sect4_pp_w4.dta") |>   #crop roster
  select(household_id, holder_id, parcel_id, field_id,crop_id, s4q01b, s4q02, s4q03) |>
  filter(s4q01b==2) # keep only maize
maizedata <- maizedata |> 
  mutate(plot_id = paste(holder_id, parcel_id, field_id, sep = "-")) # field means plot in HP
maizedata <- semi_join(maizedata, maizeplots, by="plot_id")
maizedata <- maizedata |> 
  group_by(household_id)|>
  mutate(count_plot=n())|>
  ungroup()
maizedata |>
  group_by(plot_id) |> 
  filter(n() > 1) |> 
  ungroup() # 4 observations; maize is mixed with maize for two hhs.
maizedata <- maizedata |>
   distinct(plot_id, .keep_all = TRUE) # Remove duplicates to identify maize grown fields
cropdata <- left_join(maizedata, field, by=c("household_id", "holder_id", "parcel_id", "field_id", "plot_id")) # maize fields with field data 
cropdata|>
  group_by(household_id, parcel_id, field_id) |> 
  filter(n() > 1) |> 
  ungroup() # 0 duplicates 
look_for(cropdata)
glimpse(cropdata)
cropdata4 <- cropdata |>
  mutate(rotation=ifelse(s3q34 ==1, 1, 0))
comment(cropdata4) <- "main crop maize plots"
table(cropdata4$rotation)
# 0    1 
# 1064  439 main maize fields had legume rotation
# Identify rotation at household level
rotation4 <- collap(cropdata4, rotation+count_plot~household_id, FUN=fmax)
table(rotation4$rotation)  # 332 adopters 691 non-adopters.
remove(maizedata, cropdata)
comment(rotation4) <- "hh level rotation"
var_label(rotation4$rotation) <- "If household rotated at least one maize field with legumes during the past 3y"
var_label(rotation4$count_plot) <- "number of main maize plots"
plotdata4 <- left_join(cropdata4, hpdata, by=c("plot_id")) |>
  select(-s3q03, -s3q34, -s4q02)
### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###

## DM at plot level ----
individual <- read_dta("data/eth/ess4/sect1_hh_w4.dta") |>
  mutate(female=ifelse(s1q02==2,1,0)) |>
  select(household_id, individual_id, female) |>
  rename(s3q13=individual_id)
field <- semi_join(field, maizeplots, by="plot_id") # keep only main maize plots and look at the DM on those plots.
field4 <- left_join(field, individual, by=c("household_id", "s3q13"))
remove(field)
var_label(field4$female) <- "female primary dm concern. activities on maize plot" # also on crops to be planted
field4 <- field4 |>select(household_id, parcel_id, field_id, plot_id, female)
plotdata4 <- left_join(plotdata4,field4, by=c("household_id", "parcel_id", "field_id", "plot_id"))
look_for(plotdata4)
remove(field4)
table(plotdata4$female_manager, plotdata4$female) # comparing manager and decision-maker. !there are missing values in manager!
# 0    1
# 0 1179    0
# 1    0  260 # Perfect match, managers are decision-makers. 
# but 64 missing data on both 
plotdata4 <- plotdata4 |> select(-female)

## Agri asset ownership ----
agriasset <- read_dta("data/eth/ess4/sect11_hh_w4.dta") |>
  select(household_id, asset_cd,s11q00, s11q02_1, s11q02_2, s11q02_3, s11q02_4, s11q02_5, s11q02_6) |>
  filter(asset_cd %in% c(15, 16, 30, 31, 32, 33, 34)) |># agricultural assets
  filter(s11q00==1)  # keep assets owned
agriasset <- agriasset |> 
  semi_join(maizehh, by= join_by("household_id"=="hh_id_merge")) # keep maize producers. 
agriasset <- left_join(agriasset, individual, by=c("household_id" = "household_id",
                                                   "s11q02_1"= "s3q13"))
agriasset <- left_join(agriasset, 
                       individual |>rename(female2=female),
                       by=c("household_id" = "household_id", "s11q02_2"= "s3q13"))
agriasset <- left_join(agriasset, 
                       individual |>rename(female3=female),
                       by=c("household_id" = "household_id", "s11q02_3"= "s3q13"))
agriasset <- left_join(agriasset, 
                       individual |>rename(female4=female),
                       by=c("household_id" = "household_id", "s11q02_4"= "s3q13"))
agriasset <- left_join(agriasset, 
                       individual |>rename(female5=female),
                       by=c("household_id" = "household_id", "s11q02_5"= "s3q13"))
agriasset <- left_join(agriasset, 
                       individual |>rename(female6=female),
                       by=c("household_id" = "household_id", "s11q02_6"= "s3q13"))
agriasset <- agriasset |>
  mutate(womens_asset=case_when(female==1~1,
                                female2==1~1,
                                female3==1~1,
                                female4==1~1,
                                female5==1~1,
                                female6==1~1,
                                .default = 0))
agriasset <- agriasset |>
  mutate(dm_agriasset=
           case_when(womens_asset==1 & female==1 &
                       is.na(female2) & is.na(female3) & is.na(female4) & is.na(female5) & is.na(female6) ~ 2,
                     womens_asset==1 & female==1 & 
                       (female2==1 | female3==1 | female4==1| female5==1| female6==1) ~ 2,
                     womens_asset==1 & (female==0 | female2==0 | female3==0 | female4==0| female5==0| female6==0) ~ 1,
                     .default =0))
agriasset <- agriasset |>
  group_by(household_id)|>
  mutate(dupl=n())|>
  ungroup()
table(agriasset$dupl)

# 1   2   3   4 
# 335 642 684  20

agriasset <- agriasset |>
  select(household_id, dm_agriasset, dupl)
# Function to calculate the most frequent value
mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
agriasset <- agriasset |>
  select(household_id, dm_agriasset, dupl) |>
  group_by(household_id) |>
  mutate(agri_asset=case_when(
    dupl==2 ~ max(dm_agriasset, na.rm = TRUE), # two assets: if women owned at least one asset.
    dupl>2 ~ mode_function(dm_agriasset),     # many assets: more frequent ownership type observed 
    dupl==1 ~ dm_agriasset,
    .default = NA_real_)) |>
  ungroup()
agriasset$dm_agriasset_order <- ordered(agriasset$agri_asset, 
                                        levels = c(0, 1, 2), 
                                        labels = c("male", "joint", "female"))
agriasset %>%
  group_by(household_id) %>%
  filter(n() == 2) %>%  # Keep only household_ids that appear exactly twice
  summarise(n_unique = n_distinct(dm_agriasset_order)) %>%
  filter(n_unique > 1)  # Keep only those with different values
# no hh with two type assets but different decision-maker.
agriasset <- collap(agriasset, dm_agriasset_order ~ household_id, catFUN = fmode)
remove(individual, cropdata4, mode_function)

### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###

# HDDS
# total consumption; LATER consider alternative: Nominal annual expenditure on food per adult equivalent
# consumption quantile
hhhp4 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Ethiopia", wave==4) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio,totcons_USD, cons_quint, hh_asset_index,
         HDDS, share_kg_sold)
hhhp4 <- hhhp4|> 
  semi_join(maizehh, by=c("hh_id_merge"))
look_for(hhhp4) # 297 mising for share_kg_sold
# 60 maize producing households are not in the harmonized panel

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###

## Collapse / aggregate ----
plotdata4 <- plotdata4 |> 
  select(-s3q13, -s4q03,-s4q01b)
look_for(plotdata4)
wave4 <- plotdata4 |> 
  select(1,6:11,15:42)
look_for(wave4)  
wave4mean <- collap(wave4,
                    farm_size + age_manager + dist_market + plot_dist_household + ag_asset_index + 
                      count_plot + soil_fertility_index  ~ household_id,
                    FUN = fmean,
                    na.rm = TRUE)
wave4sum <- collap(wave4,
                   plot_area_GPS +nb_seasonal_crop +yield_value_LCU + yield_value_USD  ~ household_id,
                   FUN = fsum,
                   na.rm = TRUE)
aggregated_vars <- c("farm_size", "age_manager", "dist_market", "plot_dist_household", "soil_fertility_index",
                     "ag_asset_index", "count_plot", "plot_area_GPS", "nb_seasonal_crop", 
                     "yield_value_LCU", "yield_value_USD")
wave4 <- wave4 %>% select(-all_of(aggregated_vars))
look_for(wave4)
wave4 <- wave4 |> 
  select(1, 3, 10:12, 15:24)
look_for(wave4)
# For all remaining binary variables, I will select the max. Meaning, for example,
# at least one plot is managed by female, at least one maize plot manager has primary education, 
# at least one plot is being intercropped and etc. 
wave4max <- collap(wave4,
                   ~ household_id, 
                   FUN = fmax,
                   na.rm = TRUE)
wave4 <- left_join(wave4mean, wave4sum, by=join_by("household_id"))
wave4 <- left_join(wave4, wave4max, by=join_by("household_id"))
wave4 <- left_join(wave4, agriasset, by=join_by("household_id"))
wave4 <- left_join(wave4, hhhp4, by=c("household_id"="hh_id_merge"))
var_label(wave4$dm_agriasset_order) <- "control over agricultural assets"
remove(wave4max, wave4sum,wave4mean,rotation4, agriasset, aggregated_vars, hhhp4)

## Cleaning missing data ----
look_for(wave4)
# Many missing values!!!
# This is expected for some! In page 24 and 25 of Basic Information Document, Ethiopia Socioeconomic Survey 2018-19,
# it is mentioned that in several EAs, for several households, household data is missing but agri data is included.
# for some 7 EAs post-harvest is missing and etc.   
wave4 <- wave4 |> 
  filter(!is.na(hh_size)) # household level data from HP - some HHs have agri data but no household level data4
# There are households which do not have any agricultural assets. For these asset variables will be missing. 
# I drop households which do not have any agricultural asset, because control over agricultural
# asset variable that is relevant for our analyses. 
look_for(wave4)
missing <- wave4 |> 
  filter(is.na(dm_agriasset_order)) |> 
  select(household_id)
agriasset <- read_dta("data/eth/ess4/sect11_hh_w4.dta") |>
  select(household_id, asset_cd,s11q00) |>
  filter(asset_cd %in% c(15, 16, 30, 31, 32, 33, 34))
agriasset <- semi_join(agriasset, missing, by=join_by("household_id"))
table(agriasset$s11q00) # all=2; these hhs do not have agricultural assets. 
wave4 <- wave4 |> 
  filter(!is.na(dm_agriasset_order))
look_for(wave4)
remove(missing, agriasset, maizehh, hpdata, plotdata4)
look_for(wave4)
##  age is missing for many observations in the raw data. 83 missing for manager age.
missing <- is.na(wave4$age_manager)
wave4 <- subset(wave4, subset= !missing)
wave4 <- wave4 |> mutate(wave="wave4")

### share_kg_sold----
### has many missing (276); the reason is because of many have reported share above 100% percent. 
### Also, some did not harvest any crops.
### We will capture subsistence orientation, by looking at the question 22 in section 4 that asks: Do you intend to sell any 
### of [CROP] to be harvested during the rainy season?
sec4 <- read_dta("data/eth/ess4/sect4_pp_w4.dta") |> 
  select( household_id,holder_id, parcel_id, field_id, s4q22) |> 
  mutate(plot_id = paste(holder_id, parcel_id, field_id, sep = "-"))
val_labels(sec4$s4q22) # 1 yes 2 no
sec4 <- semi_join(sec4, maizeplots, by="plot_id")
sec4 |> fgroup_by(household_id) |> fselect(s4q22) |>  fsum()
sec4 <- sec4 |> 
  mutate(subsistence_ag=if_else(s4q22==2,1,0))
sec4 %>%
  group_by(household_id) %>%
  summarise(n_unique = n_distinct(subsistence_ag, na.rm = TRUE)) %>%
  filter(n_unique > 1) # many cases where answer to the question differs. 
sec4 <- collap(sec4, subsistence_ag~household_id, FUN = fmin) # if some consider for sale then not a subsistence. 
wave4 <- left_join(wave4, sec4, by="household_id")
table(wave4$subsistence_ag)
var_label(wave4$subsistence_ag) <- "HH does not intend to sell any crop"
remove(maizeplots, missing, sec4)
look_for(wave4)
rural <- read_dta("data/eth/ess4/sect_cover_hh_w4.dta") |> 
  filter(saq14==1) # keep rural hhs.
wave4 <- semi_join(wave4, rural, by="household_id") # indeed all are rural farmers. 
remove(rural)



## Notes on missing data ----
## 1023 hhs before any cleaning of missing data
## 83 missing for manager age
## 41 missing household size, dependency ratio, consumption and some other hh level variables
## 95 hhs do not have agricultural assets but still have asset index. This is becuase, pca (factor) analysis still includes those with 
## no asset and gives them a lower score. 

