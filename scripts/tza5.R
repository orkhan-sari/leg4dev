# tza 5 ----

## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               labelled,
               collapse,
               sjmisc)
getwd()
options(scipen = 999)

# identify panel households
hh20 <- read_dta("data/tza/tza_wave5_2020_21/hh_sec_a.dta") |> 
  select(y5_hhid) |> 
  distinct(y5_hhid, .keep_all = TRUE) 


### ### ### ### ### ### ### ###
# HP data ----
### ### ### ### ### ### ### ###
hpdata <- read_dta("data/hp/Plot_dataset.dta") |> 
  filter(country == "Tanzania", wave == 5)

# We concentrate on the plots where maize is the main crop. 
maizeplots <- hpdata |> 
  filter(main_crop == "MAIZE") |> 
  select(hh_id_merge, plot_id_merge) |> 
  mutate(plot_id = plot_id_merge)

maizehh <- maizeplots |> distinct(hh_id_merge)

hpdata |> 
  filter(main_crop == "MAIZE") |> 
  group_by(hh_id_obs, plot_id_obs) |> 
  filter(n() > 1) |> 
  ungroup()  # 0 duplicates - plots are uniquely identified  

hpdata <- hpdata |> 
  filter(main_crop == "MAIZE") |> 
  select(hh_id_merge, hh_id_obs, plot_id_merge, plot_id_obs, plot_area_GPS, main_crop,
         farm_size, manager_id_merge, manager_id_obs, age_manager, female_manager, formal_education_manager, 
         primary_education_manager, respondent_id_obs, respondent_id_merge, improved, organic_fertilizer, 
         inorganic_fertilizer, soil_fertility_index, plot_dist_household, ag_asset_index, plot_certificate, 
         drought_shock, rain_shock, flood_shock, irrigated, nb_seasonal_crop, used_pesticides, yield_value_LCU, 
         yield_value_USD) |> 
  mutate(plot_id = plot_id_merge)

look_for(hpdata) 
# Five variables — respondent_id_obs, soil_fertility_index, plot_dist_household, rain_shock, and flood_shock — have substantial missing data, each with 1887 missing values.
# Variables - yield_value_LCU, yield_value_USD has 70 missing values each and drought_shock has 56 missing values.


### ### ### ### ### ### ### ###
# Maize - legume intercropping---- 
### ### ### ### ### ### ### ###
intercropdata <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_4a.dta") |>  # crops by plot
  select("y5_hhid", "plot_id", "cropid","ag4a_04") |> 
  rename(intercrop = ag4a_04) |> 
  filter(!is.na(cropid))

intercropdata <- intercropdata |> 
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-"))

intercropdata <- semi_join(intercropdata, maizeplots, by = "plot_id")
n_distinct(intercropdata$plot_id)  # 1541 distinct plots

intercropdata <- intercropdata |> 
  filter(cropid %in% c(11, 31, 32, 33, 34, 35, 36, 37, 43, 47))

# plots have a definition that is akin to parcels, and no parcels are tracked. Thus, even if a plot has three crops, it would not mean
# intercropping. Thus, we must identify plots where both maize and legumes have been reported to be intercropped. 
# In this case, we can believe that maize and legume are intercropped.

legume_codes <- c(31, 32, 33, 34, 35, 36, 37, 43, 47)

intercropdata <- intercropdata |> 
  group_by(plot_id) |> 
  mutate(
    has_maize = any(cropid == 11 & intercrop == 1),
    has_legume = any(cropid %in% legume_codes & intercrop == 1),
    intercrop_legume = as.integer(has_maize & has_legume)
  ) |> 
  ungroup()

look_for(intercropdata)

intercropdata <- collap(intercropdata, intercrop_legume ~ plot_id, FUN = fmax)
frq(intercropdata$intercrop_legume)  # 354 maize plots out of 1541 are intercropped with legumes.

# in HP data, keep only plots from the long panel (extended refresh sample not included) 
maizeplots <- semi_join(maizeplots, intercropdata, by = c("plot_id_merge" = "plot_id"))
maizehh <- maizeplots |> distinct(hh_id_merge)  # 1189 maize HHs in wave 5.
hpdata <- semi_join(hpdata, maizeplots, by = "plot_id")


### ### ### ### ### ### ### ###
# Maize - legume rotation----
### ### ### ### ### ### ### ###
# Concept of crop rotation used: If a household cultivated maize on a plot in the long rainy season, and a legume was 
# grown on the same plot in the short rainy season, then that plot experienced maize–legume rotation.
# 'plot' contains the maize plots that were actually cultivated in the long rainy season, we’ll now build on that to 
# check which of those plots were rotated to legumes in the short rainy season.

LRseason <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_4a.dta")  # Long rainy season data
SRseason <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_4b.dta")  # Short rainy season data

# Identify maize plots in the long rainy season
LRseason <- LRseason |> 
  filter(cropid == 11) |> 
  select(y5_hhid, plot_id, cropid) |> 
  mutate(plot_id_merge = paste(y5_hhid, plot_id, sep = "-"))

# Identify legume plots in the short rainy season
SRseason <- SRseason |> 
  filter(cropid %in% legume_codes) |> 
  select(y5_hhid, plot_id, cropid) |> 
  mutate(plot_id_merge = paste(y5_hhid, plot_id, sep = "-"))

# Merge the maize plots to ensure they are from households with maize as high-value crop
LRseason <- semi_join(LRseason, hpdata, by = "plot_id_merge")
SRseason <- semi_join(SRseason, hpdata, by = "plot_id_merge")

SRseason <- SRseason |> 
  mutate(rotation = 1) |> 
  select(plot_id_merge, rotation)

LRseason |> group_by(plot_id_merge) |> 
  filter(n() > 1) |> 
  ungroup()

SRseason |> group_by(plot_id_merge) |> 
  filter(n() > 1) |> 
  ungroup() 

SRseason <- SRseason |> 
  distinct(plot_id_merge, .keep_all = TRUE)

LRseason <- LRseason |> 
  select(plot_id_merge)

# Merge maize plots from the long rainy season with legume plots from the short rainy season 
LRseason <- left_join(LRseason, SRseason, by = "plot_id_merge")
frq(LRseason$rotation)  # 34 distinct plots with maize-legume rotation 
LRseason$rotation[is.na(LRseason$rotation)] <- 0

hpdata <- left_join(hpdata, LRseason, by = "plot_id_merge")
remove(LRseason, SRseason)


### ### ### ### ### ### ### ###
# Maize - legume rotation----
### ### ### ### ### ### ### ###
# Concept of crop rotation used: If a household cultivated maize on a plot in the long rainy season, and a legume was 
# grown on the same plot in the short rainy season, then that plot experienced maize–legume rotation.
# 'plot' contains the maize plots that were actually cultivated in the long rainy season, we’ll now build on that to 
# check which of those plots were rotated to legumes in the short rainy season.

LRseason <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4a.dta")  # Long rainy season data
SRseason <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4b.dta")  # Short rainy season data

# Identify maize plots in the long rainy season
LRseason <- LRseason |> 
  filter(zaocode == 11) |> 
  select(y4_hhid, plotnum, zaocode) |> 
  mutate(plot_id_merge = paste(y4_hhid, plotnum, sep = "-"))

# Identify legume plots in the short rainy season
SRseason <- SRseason |> 
  filter(zaocode %in% legume_codes) |> 
  select(y4_hhid, plotnum, zaocode) |> 
  mutate(plot_id_merge = paste(y4_hhid, plotnum, sep = "-"))

# Merge the maize plots to ensure they are from households with maize as high-value crop
LRseason <- semi_join(LRseason, hpdata, by = "plot_id_merge")
SRseason <- semi_join(SRseason, hpdata, by = "plot_id_merge")

SRseason <- SRseason |> 
  mutate(rotation = 1) |> 
  select(plot_id_merge, rotation)

LRseason |> group_by(plot_id_merge) |> 
  filter(n() > 1) |> 
  ungroup()

SRseason |> group_by(plot_id_merge) |> 
  filter(n() > 1) |> 
  ungroup()  # several duplicates - intercropped. 

SRseason <- SRseason |> 
  distinct(plot_id_merge, .keep_all = TRUE)

LRseason <- LRseason |> 
  select(plot_id_merge)

# Merge maize plots from the long rainy season with legume plots from the short rainy season 
LRseason <- left_join(LRseason, SRseason, by = "plot_id_merge")
frq(LRseason$rotation)  # 37 distinct plots with maize-legume rotation across 33 households
LRseason$rotation[is.na(LRseason$rotation)] <- 0

hpdata <- left_join(hpdata, LRseason, by = "plot_id_merge")
remove(LRseason, SRseason)

### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###

modd <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_3a.dta") |> 
  select(y5_hhid, plot_id, ag3a_08a_1) |>
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-")) |> 
  rename(id_code=ag3a_08a_1)
modd <- semi_join(modd, maizeplots, by="plot_id")
individual <- read_dta("data/tza/tza_wave5_2020_21/hh_sec_b.dta") |> 
  select(y5_hhid, indidy5, hh_b02, hh_b04) |> 
  mutate(female=ifelse(hh_b02==2,1,0)) 

# Rename 'indidy5' to 'id_code' in the 'individual' dataset
individual <- individual |> 
  rename(id_code = indidy5)

modd <- left_join(modd,individual, by=c("y5_hhid", "id_code"))
summary(modd)
remove(individual)
var_label(modd$female) <- "female primary dm concern. activities on maize plots" # also on crops to be planted

modd <- modd |> select(plot_id, female,hh_b04 )  
plot5 <- left_join(maizeplots, modd, by="plot_id")
plot5 <- plot5 |> rename(age_manager_row=hh_b04)
remove(modd)

## Asset ownership ---- 
## no data available on owners - there is a section on household assets, but no info on ownership.


## Education ----
## variable of interest: if HH has an educated adult female

# Load individual data with sex, age, relationship to head
individual <- read_dta("data/tza/tza_wave5_2020_21/hh_sec_b.dta") |> 
  select(y5_hhid, indidy5, hh_b02, hh_b04, hh_b05) |> 
  mutate(female = ifelse(hh_b02 == 2, 1, 0)) |> 
  filter(female == 1) |>
  filter(hh_b04 > 17) |>
  rename(id_code = indidy5)
# Merge with education data (highest education level completed)
edu <- read_dta("data/tza/tza_wave5_2020_21/hh_sec_c.dta") |> 
  select(y5_hhid, indidy5, hh_c07) |> 
  rename(id_code = indidy5)
individual <- left_join(individual, edu, by = c("y5_hhid", "id_code"))

# Keep only maize-growing households
individual <- semi_join(individual, maizehh, by = c("y5_hhid" = "hh_id_merge"))

frq(individual$hh_c07)  # D7 - Class 7, D4- Class 4, F4-Form 4
look_for(individual)

individual$hh_c07 <- as.numeric(individual$hh_c07)

# Collapse to HH level – keep max female education in HH
individual <- collap(individual, hh_c07 ~ y5_hhid, FUN = fmax, na.rm = TRUE)


# ---------------------------------------------------------------------------- #
# Doubt: How to create binary indicator – 1 = educated, 0 = no/low education ?

# In the Tanzania dataset, the variable `hh_c07` (highest education completed)
# doesn't seem to include a specific code for "no education." The values start from:
#   1 = pp (pre-primary), 2 = adult education, 
#   11–17 = D1–D7 (Primary grades 1–7), 
#   18 = D8 (Primary grade 8), 
#   19 = Pre-Form 1,
#   21–25 = F1–F4 (Form 1–4), 
#   31+ = Higher secondary, diploma, university, etc. 
#
# Since we are focusing on adult females (age > 17), one suggestion could be to 
# classify individuals with `hh_c07` >= 11 as "educated"—i.e., those who completed 
# at least some formal primary schooling. Values below that (like 1 or 2), or missing, 
# could be considered as not formally educated.
#
# Please let me know if this classification makes sense, or if you'd suggest using 
# a different threshold.
# ---------------------------------------------------------------------------- #


### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
hdds5 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Tanzania", wave==5) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, hh_asset_index, HDDS, share_kg_sold)
hdds4 <- hdds5|> 
  semi_join(maizehh, by="hh_id_merge")



### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###
plot5 <- left_join(plot5, hpdata, by = "plot_id")
look_for(plot5)

all_identical <- all(plot5$hh_id_merge.x == plot5$hh_id_merge.y)
print(all_identical)
plot4 <- plot5[, !names(plot5) %in% c("hh_id_merge.y", "plot_id_merge.y")]
names(plot5) <- gsub("\\.x$", "", names(plot5))


look_for(plot5)
remove(hpdata)


plot5 <- plot5 |> rename(hhid=hh_id_merge)
hh20 <- hh20 |> rename(hhid = y5_hhid)
plot5 <- left_join(plot5, hh20, by="hhid")

table(plot5$female_manager, plot5$female) # same information.
mismatched_rows <- plot5 |> filter(female_manager != female)
View(mismatched_rows) 

# --------------------------DOUBT--------------------------------------------- #
# Checking consistency between 'female_manager' and 'female' after merging
# Found 8 mismatches:
# - 1 case where female_manager = 0 but female = 1
# - 7 cases where female_manager = 1 but female = 0
# Couldn’t identify what went wrong 
# ---------------------------------------------------------------------------- #

plot5 <- plot5 |> select(-female)
look_for(plot5)
# manager age is missing for 4 plots.

## Collapse / aggregate ----
tza5 <- plot5 |> 
  select(-plot_id_merge, -plot_id,  -respondent_id_obs, -respondent_id_merge,-age_manager_row)
look_for(tza5)
tza5basics <- tza5 |> 
  select(hhid, hh_id_obs) |> 
  distinct()
wave5mean <- collap(tza5,
                    farm_size + age_manager + plot_dist_household + ag_asset_index + 
                      soil_fertility_index  ~ hhid,
                    FUN = fmean,
                    na.rm = TRUE)
wave5sum <- collap(tza5,
                   plot_area_GPS + nb_seasonal_crop +yield_value_LCU + yield_value_USD  ~ hhid,
                   FUN = fsum,
                   na.rm = TRUE)
aggregated_vars <- c("farm_size", "age_manager", "plot_dist_household", "soil_fertility_index",
                     "ag_asset_index", "plot_area_GPS", "nb_seasonal_crop", 
                     "yield_value_LCU", "yield_value_USD")
tza5 <- tza5 %>% select(-all_of(aggregated_vars))
look_for(tza5)

# --------------------------DOUBT--------------------------------------------- #
#Not sure why only columns 1, 6–7, and 12–23 are being selected here — unclear which variables are intended or why others are dropped
tza5 <- tza5 |> 
  select(1, 6:7, 12:23)
# ---------------------------------------------------------------------------- #

look_for(tza5)
# For all remaining binary variables, I will select the max. Meaning, for example,
# at least one plot is managed by female, at least one maize plot manager has primary education, 
# at least one plot is being intercropped and etc. 
tza5 <- tza5 |> 
  mutate(across(c(female_manager, formal_education_manager, primary_education_manager,
                  inorganic_fertilizer, plot_certificate, irrigated),
                as.numeric))
wave5max <- collap(tza5,
                   ~ hhid, 
                   FUN = fmax,
                   na.rm = TRUE)
tza5 <- left_join(tza5basics, wave5mean, by=join_by("hhid"))
tza5 <- left_join(tza5, wave5sum, by=join_by("hhid"))
tza5 <- left_join(tza5, wave5max, by=join_by("hhid"))
tza5 <- left_join(tza5, hdds5, by=c("hhid"="hh_id_merge"))
tza5<- left_join(tza5, individual, by=c("hhid"="y5_hhid"))
tza5 <- tza5 |> 
  mutate(female_edu=if_else(is.na(female_edu), 0, female_edu))  ####error since female_edu is not defined in the previous step
remove(tza5basics, wave5max, wave5sum,wave5mean,aggregated_vars, hdds5, plot5, individual)
tza5 <- tza5 |> mutate(wave="wave5")



## Cleaning missing data ----

#################################################################################
### ### ### ### ### ### ### ###
# HH Plot distance
### ### ### ### ### ### ### ###

plot_hh_dist <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_3a.dta") |>  
  select(y5_hhid, plot_id, ag3a_02_1) |>  
  rename(plot_hh_distance = ag3a_02_1) |>  
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-"))

plot_hh_dist <- semi_join(plot_hh_dist, maizeplots, by = "plot_id")

attr(plot_hh_dist$plot_hh_distance, "label") <- "Distance from plot to home (in KMs), self-reported"

look_for(plot_hh_dist)
# No missing values found for plot_hh_distance


### ### ### ### ### ### ### ###
# Soil Type
### ### ### ### ### ### ### ###

soil_type <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_3a.dta") |>  
  select(y5_hhid, plot_id, ag3a_10) |>  
  rename(soil_type = ag3a_10) |>  
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-"))

soil_type <- semi_join(soil_type, maizeplots, by = "plot_id")

look_for(soil_type)  # No missing values found for soil_type

soil_type |> 
  count(soil_type, sort = TRUE)


### ### ### ### ### ### ### ###
# Subsistence orientation
### ### ### ### ### ### ### ###

# Identify households with a subsistence orientation (did not sell any maize in 2014)
subsistence_ag <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_5a.dta") |> 
  select(hh_id_merge = y5_hhid, sell_produce = ag5a_01) |> 
  semi_join(maizehh, by = "hh_id_merge")

# Count households by selling status: 1 = Sold, 2 = Did not sell, NA = Missing
subsistence_ag |> 
  count(sell_produce) 

# Filter households who did not sell maize (sell_produce == 2)
subsistence_only <- subsistence_ag |> 
  filter(sell_produce == 2)

look_for(hdds4)  # 507 missing for share_kg_sold


### ### ### ### ### ### ### ### #### ##### #### ### ###
# Auxiliary Check: How Many LR Legume-Growing Households Planted Maize in the SR
### ### ### ### ### ### ### ###### ### ###  ### ### ### 

legume_codes <- c(31, 32, 33, 34, 35, 36, 37, 43, 47)

# Households that grew legumes in the Long Rainy Season
LR_legume_hhs <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_4a.dta") |> 
  filter(cropid %in% legume_codes) |> 
  distinct(y5_hhid)

# Households that grew maize in the Short Rainy Season
SR_maize_hhs <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_4b.dta") |> 
  filter(cropid == 11) |> 
  distinct(y5_hhid)

# How many legume-growing HHs in LR also grew maize in SR?
legume_then_maize_hhs <- inner_join(LR_legume_hhs, SR_maize_hhs, by = "y5_hhid")

n_LR_legume_hhs <- nrow(LR_legume_hhs)
n_LR_legume_and_SR_maize_hhs <- nrow(legume_then_maize_hhs)

cat("Total households that grew legumes in LR:", n_LR_legume_hhs, "\n")
cat("Of them, households that also grew maize in SR:", n_LR_legume_and_SR_maize_hhs, "\n")

# Out of 957 households that grew legumes in the Long Rainy Season (LR),
# 115 also grew maize in the Short Rainy Season (SR)
# Maybe this suggests that most of these households were likely primarily legume growers,
# with limited transition to maize in SR.


