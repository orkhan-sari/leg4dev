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
intercropdata <- intercropdata |> 
  group_by(y5_hhid)|>
  mutate(count_plot=n())|>
  ungroup()

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

intercropdata <- collap(intercropdata, intercrop_legume + count_plot ~ plot_id, FUN = fmax)
frq(intercropdata$intercrop_legume)  # 354 maize plots out of 1541 are intercropped with legumes.

# in HP data, keep only plots from the long panel (extended refresh sample not included) 
maizeplots <- semi_join(maizeplots, intercropdata, by = "plot_id")
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
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-"))

# Identify legume plots in the short rainy season
SRseason <- SRseason |> 
  filter(cropid %in% legume_codes) |> 
  select(y5_hhid, plot_id, cropid) |> 
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-"))

# Merge the maize plots to ensure they are from households with maize as high-value crop
LRseason <- semi_join(LRseason, hpdata, by = "plot_id")
SRseason <- semi_join(SRseason, hpdata, by = "plot_id")

SRseason <- SRseason |> 
  mutate(rotation = 1) |> 
  select(plot_id, rotation)

LRseason |> group_by(plot_id) |> 
  filter(n() > 1) |> 
  ungroup()

SRseason |> group_by(plot_id) |> 
  filter(n() > 1) |> 
  ungroup() 

SRseason <- SRseason |> 
  distinct(plot_id, .keep_all = TRUE)

LRseason <- LRseason |> 
  select(plot_id)

# Merge maize plots from the long rainy season with legume plots from the short rainy season 
LRseason <- left_join(LRseason, SRseason, by = "plot_id")
frq(LRseason$rotation)  # 34 distinct plots with maize-legume rotation 
LRseason$rotation[is.na(LRseason$rotation)] <- 0

plot5 <- left_join(intercropdata, LRseason, by = "plot_id")
remove(LRseason, SRseason, intercropdata)

## improved maize  ----
## here we don't have the info at parcel level but only at hh level. all seeds were asked. We concentrate on maize. 
seed <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_4a.dta") |> 
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-")) |> 
  select(y5_hhid, plot_id, cropid, ag4a_06, ag4a_08) |> 
  filter(cropid==11)
look_for(seed)
seed |> group_by(y5_hhid) |> 
  filter(n()>1) |>
  ungroup() # improved varieties in some fields of HHs. 
# HHs have several maize fields,and it is not possible to identify 
# in which parcel the improved seed was cultivated, i.e., rotated parcel or not.** 
frq(seed$ag4a_08)
seed <- seed |>
  mutate(improved_maize=if_else(ag4a_08 %in% c(1,3),1,0))
frq(seed$improved_maize)
seed <- seed |> 
  group_by(plot_id) |> 
  summarise(improved_maize=max(improved_maize)) |> 
  ungroup()
plot5 <- left_join(plot5, seed, by="plot_id")
frq(plot5$improved_maize)
remove(seed)

### ### ### ### ### ### ### ###
# Soil management ----
# crop residue short season
plots <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_4b.dta") |> 
  select(y5_hhid, plot_id, cropid) |> 
  filter(!is.na(cropid)) |> 
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-"))
look_for(plots)
plots <- plots |> 
  distinct(y5_hhid, cropid, .keep_all = TRUE)
soilman <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_5b.dta") |> # crop level no plot key 
  select(y5_hhid, ag5b_33, cropid)
look_for(soilman)
frq(soilman$ag5b_33)
soilman <- soilman |>
  filter(cropid %in% c(11, 31, 32, 33, 34, 35, 36, 37, 43, 47)) # keep only maize and legume
soilman <- soilman |>
  filter(!is.na(ag5b_33)) |> 
  mutate(crop_residue=if_else(ag5b_33==2, 1, 0)) |> 
  select(-ag5b_33)
soilman <- left_join(soilman, plots, by=c("y5_hhid", "cropid"))
soilman <- soilman |> 
  group_by(plot_id) |> 
  summarise(crop_residue_short=max(crop_residue)) |> 
  ungroup()
plot5 <- left_join(plot5, soilman, by="plot_id")
frq(plot5$crop_residue_short)
plot5$crop_residue_short[is.na(plot5$crop_residue_short)] <- 0
var_label(plot5$crop_residue_short) <- "Residues from short rainy season were left in the field"

# residue long season
plots <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_4a.dta") |> 
  select(y5_hhid, plot_id, cropid) |> 
  filter(!is.na(cropid)) |> 
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-"))
# for long season we concentrate on maize and legumes to possible be able ot link to the fields of interest as there is no direct key
plots <- plots |>
  filter(cropid %in% c(11, 31, 32, 33, 34, 35, 36, 37, 43, 47))
plots <- plots |> 
  distinct(y5_hhid, cropid, .keep_all = TRUE)
soilman <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_5a.dta") |> # crop level no plot key 
  select(y5_hhid, ag5a_33, cropid)
soilman <- soilman |>
  filter(cropid %in% c(11, 31, 32, 33, 34, 35, 36, 37, 43, 47))
look_for(soilman)
soilman <- soilman |>
  filter(!is.na(ag5a_33)) |> 
  mutate(crop_residue=if_else(ag5a_33==2, 1, 0)
  )
soilman <- left_join(soilman, plots, by=c("y5_hhid", "cropid"))
soilman <- soilman |> 
  group_by(plot_id) |> 
  summarise(crop_residue_long=max(crop_residue)) |> 
  ungroup()
plot5 <- left_join(plot5, soilman, by="plot_id")
frq(plot5$crop_residue_long)
plot5$crop_residue_long[is.na(plot5$crop_residue_long)] <- 0
var_label(plot5$crop_residue_long) <- "Residues from the latest long season were left in the field"
rm(soilman, plots)

## Soil Type ----
soil_type <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_3a.dta") |>  
  select(y5_hhid, plot_id, ag3a_10, ag3a_11, ag3a_12) |>
  rename(soil_type = ag3a_10) |>  
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-"))
soil_type <- semi_join(soil_type, maizeplots, by = "plot_id")
look_for(soil_type)  # No missing values in soil_type
soil_type |>
  count(y5_hhid) |>
  filter(n > 1)
frq(soil_type$soil_type) # not clear what other types would be. this is always a problem
frq(soil_type$ag3a_11) # very subjective of course
frq(soil_type$ag3a_12)
# loam is more dominant, sandy is less fertile, clay is suitable for some crops like rice. let's take loam as main.
soil_type <- soil_type |> 
  mutate(soil_type=as.numeric(soil_type),
         soil=if_else(soil_type==2,1,0)) |> 
  select(plot_id, soil)
n_distinct(soil_type$plot_id)
plot5 <- left_join(plot5, soil_type, by="plot_id")
var_label(plot5$soil) <-  "loam soil type"
remove(soil_type)

## Slop ----
slope <-  read_dta("data/tza/tza_wave5_2020_21/ag_sec_3a.dta") |> 
  select(y5_hhid, plot_id, ag3a_17) |>
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-"))
slope <- semi_join(slope, maizeplots, by="plot_id")
frq(slope$ag3a_17)
slope <- slope |> 
  mutate(flat_slope=case_when(ag3a_17 %in% c(1,2) ~ 1,
                              .default=0)
  )
frq(slope$flat_slope)
n_distinct(slope$plot_id)
look_for(slope)
slope <- slope |> select(plot_id, flat_slope)
plot5 <- left_join(plot5, slope, by="plot_id")
var_label(plot5$flat_slope) <- "flat slope"
remove(slope)

### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###
## DM at plot level ----
modd <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_3a.dta") |> 
  select(y5_hhid, plot_id, ag3a_08a_1) |>
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-")) |> 
  rename(id_code=ag3a_08a_1) |> 
  filter(!is.na(id_code))
modd <- semi_join(modd, maizeplots, by="plot_id")
individual <- read_dta("data/tza/tza_wave5_2020_21/hh_sec_b.dta") |> 
  select(y5_hhid, indidy5, hh_b02, hh_b04) |> 
  mutate(female=ifelse(hh_b02==2,1,0)) 
individual <- individual |> 
  rename(id_code = indidy5)
modd <- left_join(modd,individual, by=c("y5_hhid", "id_code"))
summary(modd)
var_label(modd$female) <- "female primary dm concern. activities on maize plots" # also on crops to be planted
modd <- modd |> select(plot_id, female,hh_b04 )  
plot5 <- left_join(plot5, modd, by="plot_id")
plot5 <- plot5 |> rename(age_manager_row=hh_b04)
remove(modd)

## DM on crop income ----
sales <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_5a.dta") |> 
  select(y5_hhid, ag5a_10_1, ag5a_10_2) |> 
  filter(ag5a_10_1==1)
look_for(sales)
sales <- semi_join(sales, maizehh, by=c("y5_hhid"="hh_id_merge"))
sales <- sales |> 
  rename(id_code=ag5a_10_1)
individual <- individual |> 
  select(y5_hhid, id_code, female)
sales <- left_join(sales,individual, by=c("y5_hhid", "id_code"))
look_for(sales)
sales$id_code <- NULL
sales <- sales |> 
  rename(id_code=ag5a_10_2) # now second decision-maker
sales <- left_join(sales,individual, by=c("y5_hhid", "id_code"))
look_for(sales)
sales <- sales |> 
  mutate(female_dm=case_when(female.x==1 & female.y==1 ~ 3, #female
                             female.x==1 & is.na(female.y) ~ 3, #female
                             is.na(female.x) & female.y==1 ~ 3, #female
                             female.x==0 & female.y==1 ~ 2,
                             female.x==1 & female.y==0 ~ 2, # joint
                             female.x==0 & female.y==0 ~ 1, # male
                             is.na(female.x) & female.y==0 ~ 1, # male
                             female.x==0 & is.na(female.y) ~1, 
                             .default = 99)
  )
frq(sales$female_dm)
look_for(sales)
sales <- sales[,c(1,5)]
n_distinct(sales$y5_hhid)
sales <- collap(sales, female_dm ~ y5_hhid, FUN = fmax)
sales <- sales |> 
  mutate(female_dm= factor(female_dm, levels = c(1,2,3), labels = c("Female", "Joint", "Male")))
frq(sales$female_dm)
rm(individual)

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
remove(edu)
# Keep only maize-growing households
individual <- semi_join(individual, maizehh, by = c("y5_hhid" = "hh_id_merge"))
frq(individual$hh_c07)  # D7 - Class 7, D4- Class 4, F4-Form 4
look_for(individual)

individual$hh_c07 <- as.numeric(individual$hh_c07)
# Collapse to HH level – keep max female education in HH
individual <- collap(individual, hh_c07 ~ y5_hhid, FUN = fmax, na.rm = TRUE)
# We will focus on primary education:primary edu is 7 years in TZ; so >16
individual <- individual |> 
  mutate(female_edu=ifelse(hh_c07 > 16,1,0)) |> # =1 if hh has educated female with at least primary education completed. 
  filter(!is.na(hh_c07)) |> 
  select(y5_hhid, female_edu)

### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
hdds5 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Tanzania", wave==5) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, hh_asset_index, HDDS, share_kg_sold)
hdds5 <- hdds5|> 
  semi_join(maizehh, by="hh_id_merge")

# FCS
fcs <- read_dta("data/tza/tza_wave5_2020_21/hh_sec_j3.dta")
look_for(fcs)
fcs$itemcode <- as.factor(fcs$itemcode)
fcs <- fcs |> 
  group_by(y5_hhid) %>%
  mutate(
    sum_A_B = sum(hh_j08[itemcode %in% c(1, 2)], na.rm = TRUE),
    sum_A_B = case_when(sum_A_B > 7 ~ 7,
                        .default = sum_A_B),
    count = case_when(itemcode== 1 ~ sum_A_B,
                      .default = hh_j08)
  ) |> 
  ungroup()
fcs <- fcs |> 
  filter(itemcode != 2)
table(fcs$itemcode,fcs$count)
fcs <- fcs |> 
  mutate(count=if_else(count > 7, 7, count))
fcs <- fcs |> 
  mutate(total=case_when(itemcode == 1 ~ count * 2,
                         itemcode == 3 ~ count * 3,
                         itemcode == 4 ~ count * 1,
                         itemcode == 5 ~ count * 4,
                         itemcode == 6 ~ count * 1,
                         itemcode == 7 ~ count * 4,
                         itemcode == 8 ~ count * 0.5,
                         itemcode == 9 ~ count * 0.5,
                         itemcode == 10 ~ count * 0)
  )
fcs <- fcs |> 
  group_by(y5_hhid) |> 
  summarise(fcs=sum(total)) |> 
  ungroup()
fcs <- semi_join(fcs, maizehh, by=c("y5_hhid" = "hh_id_merge"))
hdds5 <- left_join(hdds5, fcs, by=c("hh_id_merge" = "y5_hhid"))
summary(hdds5$fcs)
rm(fcs)

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###
plot5 <- left_join(plot5, hpdata, by = "plot_id")
look_for(plot5)
remove(hpdata)
plot5 <- plot5 |> rename(hhid=hh_id_merge)
table(plot5$female_manager, plot5$female)
mismatched_rows <- plot5 |> filter(female_manager != female)
View(mismatched_rows) 
# --------------------------DOUBT--------------------------------------------- #
# Checking consistency between 'female_manager' and 'female' after merging
# Found 8 mismatches:
# - 33 case where female_manager = 0 but female = 1
# - 60 cases where female_manager = 1 but female = 0
# Both are kept! 
# ---------------------------------------------------------------------------- #
remove(mismatched_rows)
look_for(plot5)
# manager age is missing for 4 plots.

## Collapse / aggregate ----
tza5 <- plot5 |> 
  select(-plot_id_merge, -respondent_id_obs, -respondent_id_merge,-age_manager_row)
look_for(tza5)
tza5 <- tza5 |> select(11:12, 1:10, 14, 16:37)
n_distinct(tza5$hh_id_obs) # 1189 hhs, 1541 obs
tza5 <- tza5 |> 
  group_by(hh_id_obs) |> 
  mutate(plot_area=sum(plot_area_GPS, na.rm = TRUE),
         yield_value_USD=sum(yield_value_USD, na.rm = TRUE),
         yield_value_LCU=sum(yield_value_LCU, na.rm = TRUE)) |> 
  ungroup() |> 
  relocate(plot_area, .after = plot_area_GPS)
# control if hh used any organic or inorganic fertilizer in intercrop field or any other field.
table(tza5$rotation, tza5$intercrop_legume) # intercroppping more common than rotation
tza5 <- tza5 |> 
  rename(intercrop=intercrop_legume) |> 
  group_by(hh_id_obs) |> 
  mutate(iv_maize = max(improved_maize),
         inorganic_fertilizer = max(inorganic_fertilizer, na.rm = TRUE),
         organic_fertilizer = max(organic_fertilizer, na.rm = TRUE),
         rotation_check=max(rotation, na.rm = TRUE))|> 
  ungroup()
### aggregating the information at the household level for adopters and non-adopters seperately.----
### 1. Select adopted plots If there are few intercropped fields select the biggest and 
### adjust if rotation was practiced in any field of the hh
### 2. Among the non-adopted plots select the biggest field. 
n_distinct(tza5$plot_id) # 1541 distinct plots as obs count 
table(tza5$intercrop, tza5$rotation) # 1165 non adopted plots
tza5adoption <- tza5 |>
  relocate(intercrop, .after = rotation) |> 
  mutate(keep=if_else(intercrop==1 & rotation==1, 1, 0))
# keep only adopting households; for non-adopters will select the biggest parcel and manager characteristics
tza5adoption <- tza5adoption |> 
  group_by(hh_id_obs) |> 
  filter(intercrop != 0 | rotation != 0) |> 
  ungroup()
table(tza5adoption$intercrop, tza5adoption$rotation)
# first keep these plots and hhs, and drop if there is another plot where one practice is practiced
tza5adoption <- tza5adoption |> 
  group_by(hh_id_obs) |> 
  mutate(keephh=max(keep)) |> 
  ungroup()
tza5adoption <- tza5adoption |>
  filter((keep==0 & keephh==0) | (keep==1 & keephh==1)) 
tza5adoption$keephh <- NULL
tza5adoption <- tza5adoption |> 
  group_by(hh_id_obs) |> 
  filter(intercrop==max(intercrop, na.rm = TRUE )) |>  # there were two parcels that had been rotated but not intercropped within hh
  ungroup()
tza5adoption <- tza5adoption |>
  mutate(rotation=rotation_check) # for the indication of adoption
n_distinct(tza5adoption$hh_id_obs) # 338 hhs
# keep the biggest plot
tza5adoption <- tza5adoption |> 
  group_by(hh_id_obs) |> 
  filter(plot_area_GPS == max(plot_area_GPS, na.rm = TRUE)) |> 
  ungroup()

# select the biggest plot among non-adopters
tza5 <- tza5 |> 
  filter(intercrop == 0 & rotation == 0) 
tza5 <- tza5 |> 
  group_by(hh_id_obs) |> 
  filter(plot_area_GPS == max(plot_area_GPS, na.rm = TRUE)) |> 
  ungroup()
n_distinct(tza5$hh_id_obs) # 927 hh 930 obs
tza5 <- tza5 |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs)
# trying complete cases to see if any hh will be dropped at the end. if this results in dropping a hh,
# approach is changed later
tza5 <- tza5 |> # seelcting neares plot, differences are often meager
  group_by(hh_id_obs) |>  
  filter(
    (dupl > 1 & crop_residue_long==max(crop_residue_long, na.rm=TRUE)) |
      # if hh has no duplicates → keep all rows
      (dupl == 1)
  ) |> 
  ungroup()
tza5 <- tza5 |> select(-dupl) |>
  group_by(hh_id_obs) |> mutate(dupl=n()) |> 
  relocate(dupl, .after=hhid) |>  ungroup() 
# no difference in the remaining one duplicate 
tza5 <- tza5 |> select(-dupl) |> distinct(hh_id_obs, .keep_all = TRUE) 
# Combine adopter and non-adopter hhs
tza5 <- bind_rows(tza5, tza5adoption)
n_distinct(tza5$hh_id_obs) # 1189 hhs and 1265 obs
tza5 <- tza5 |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs) |> 
  arrange(desc(dupl), hh_id_obs)
# There are hhs, which have adopter plots and non-adopted plots. keep only adopted plots
tza5 <- tza5 |> 
  group_by(hh_id_obs) |> 
  filter((dupl>1 & (intercrop==1 | rotation==1)) |
           dupl==1) |> ungroup()
tza5 <- tza5 |> 
  select(-dupl, -rotation_check, -keep) |> 
  arrange(hh_id_obs)
look_for(tza5) 
var_label(tza5$plot_area) <- "Area (in hectares) of all maize plots"
labs <- var_label(plot5)
common_names <- intersect(names(tza5), names(labs))
var_label(tza5)[common_names] <- labs[common_names]
look_for(tza5) 
rm(labs, tza5adoption, common_names)
tza5$plot_area_GPS <- NULL
tza5$improved_maize <- NULL
look_for(tza5)
var_label(tza5$rotation) <- "HH rotated at least one maize field with legumes during the previous short season"
var_label(tza5$count_plot) <- "number of main maize plots"
var_label(tza5$iv_maize) <- "HH has cultivated improved maize in at least one field"
var_label(tza5$organic_fertilizer) <- "organig fertilizer applied to at least one maize plot"
var_label(tza5$inorganic_fertilizer) <- "inorganig fertilizer applied to at least one maize plot"
tza5<- left_join(tza5, sales, by=c("hhid"="y5_hhid"))
frq(tza5$female_dm) 
var_label(tza5$female_dm) <- "Who kept/decided what to do with crop income?"
tza5 <- left_join(tza5, hdds5, by=c("hhid"="hh_id_merge"))
tza5<- left_join(tza5, individual, by=c("hhid"="y5_hhid"))
tza5 <- tza5 |> 
  mutate(female_edu=if_else(is.na(female_edu), 0, female_edu))
remove(hdds5, plot5, individual)
tza5 <- tza5 |> mutate(wave="wave5")

## Cleaning missing data ----
look_for(tza5)
### share_kg_sold----
# Subsistence orientation
# Identify households with a subsistence orientation (did not sell any maize in 2020)
subsistence_ag <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_5a.dta") |> 
  select(hh_id_merge = y5_hhid, ag5a_01) |> 
  semi_join(maizehh, by = "hh_id_merge") |> 
  mutate(subsistence_ag=if_else(ag5a_01==2,1,0)) |> # sell_produce == 2
  rename(hhid=hh_id_merge)
frq(subsistence_ag$subsistence_ag)
subsistence_ag <- collap(subsistence_ag, subsistence_ag~hhid, FUN = fmin, na.rm = TRUE) # min captures if any crop was sold
frq(subsistence_ag$subsistence_ag) # 442 are subsistence oriented
tza5 <- left_join(tza5, subsistence_ag, by="hhid")
frq(tza5$subsistence_ag) # 21 still missing. no harvest? 
remove(subsistence_ag)

### ### ### ### ### ### ### ###
# Other variables  ----
### ### ### ### ### ### ### ###

### ### ### ### ### ### ### ###
## Distance from Plot to Home, Road and Market ----

distance_from_plot <- "data/tza/tza_wave5_2020_21/ag_sec_3a.dta"

# ───── Plot-to-home distance ─────
plot_hh_dist <- read_dta(distance_from_plot) |>  
  select(y5_hhid, plot_id, ag3a_02_1) |>  
  rename(plot_hh_distance = ag3a_02_1) |>  
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-")) |>  
  semi_join(maizeplots, by = "plot_id")
attr(plot_hh_dist$plot_hh_distance, "label") <- "Distance from plot to home (in KMs), self-reported"
look_for(plot_hh_dist)
plot_hh_dist |>
  count(y5_hhid) |>
  filter(n > 1)
plot_hh_dist<- plot_hh_dist |>
  group_by(y5_hhid) |>
  summarise(plot_hh_dist_mean = mean(plot_hh_distance, na.rm = TRUE)) |> 
  ungroup()
tza5 <- left_join(tza5, plot_hh_dist, by = c("hhid" = "y5_hhid"))

# ───── Plot-to-road distance ─────
plot_road_dist <- read_dta(distance_from_plot) |>  
  select(y5_hhid, plot_id, ag3a_02_2) |>  
  rename(plot_road_distance = ag3a_02_2) |>  
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-")) |>  
  semi_join(maizeplots, by = "plot_id")
attr(plot_road_dist$plot_road_distance, "label") <- "Distance from plot to road (in KMs), self-reported"
look_for(plot_road_dist)
plot_road_dist |>
  count(y5_hhid) |>
  filter(n > 1)
plot_road_dist<- plot_road_dist |>
  group_by(y5_hhid) |>
  summarise(plot_road_dist_mean = mean(plot_road_distance, na.rm = TRUE)) |> ungroup()
tza5 <- left_join(tza5, plot_road_dist, by = c("hhid" = "y5_hhid"))

# ───── Plot-to-market distance ─────
plot_market_dist <- read_dta(distance_from_plot) |>  
  select(y5_hhid, plot_id, ag3a_02_3) |>  
  rename(plot_market_distance = ag3a_02_3) |>  
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-")) |>  
  semi_join(maizeplots, by = "plot_id")
attr(plot_market_dist$plot_market_distance, "label") <- "Distance from plot to market (in KMs), self-reported"
look_for(plot_market_dist)
plot_market_dist |>
  count(y5_hhid) |>
  filter(n > 1)
plot_market_dist<- plot_market_dist |>
  group_by(y5_hhid) |>
  summarise(plot_market_dist_mean = mean(plot_market_distance, na.rm = TRUE)) |> 
  ungroup()
tza5 <- left_join(tza5, plot_market_dist, by = c("hhid" = "y5_hhid"))
remove(plot_hh_dist, plot_market_dist, plot_road_dist, distance_from_plot)

## Non-farm income ----
wage <- read_dta("data/tza/tza_wave5_2020_21/hh_sec_e1.dta")
look_for(wage, details = "none")
wage <- wage[, c(1:3, 7:12, 53:63, 84:93)]
wage <- semi_join(wage, maizehh, by = c("y5_hhid" = "hh_id_merge"))
look_for(wage, details = "none")
mainwage <- wage[, c(2, 10:20)]
mainwage <- mainwage |> filter(hh_e33==1)
look_for(mainwage, details = "none")
frq(mainwage$hh_e35b)
mainwage <- mainwage |>
  mutate(
    hh_e35a = as.numeric(hh_e35a),   # cash wage amount
    hh_e35b = as.numeric(hh_e35b),   # cash wage time unit
    hh_e37a = as.numeric(hh_e37a),   # in-kind wage amount
    hh_e37b = as.numeric(hh_e37b),   # in-kind wage time unit
    hh_e38  = as.numeric(hh_e38),     # months worked
    hh_e39   = as.numeric(hh_e39),     # weeks per month
    hh_e40   = as.numeric(hh_e40)      # hours per week
  )
frq(mainwage$hh_e35b)

# If the individual's last payment covers last month and , let's say he worked for 4 months during past year,
# we can assume his yearly income is last month payment amount multiplied by 4 (months worked). Conversion to
# hourly rates is not necessary if the payment covers a different period than a day or hour. The difference
# between mlw and tzn is that in mlw, individual could say that the last payment covered for example past two 
# months or weeks or etc, but in tnz it is just a month - no amount but just unit question - hh_e26_2. 
# Moreover, for example if the individual's last payment was reported for a day and he worked 2 month, 2 weeks
# each month and 40 hours per week. His total hours worked would be 2*2*40/8 expressed in days. 

mainwage <- mainwage |> 
  mutate(main_income=case_when(hh_e35b==5 ~ hh_e35a * hh_e38,
                               hh_e35b==8 ~ hh_e35a,
                               hh_e35b==6 ~ hh_e35a * hh_e38/4, #some worked only three month but got paid for quarter?!
                               hh_e35b==4 ~ hh_e35a * hh_e38 * hh_e39 / 2, # total worked weeks divided by 2
                               hh_e35b==3 ~ hh_e35a * hh_e38 * hh_e39, 
                               hh_e35b==2 ~ hh_e35a * hh_e38 * hh_e39 * hh_e40/8,
                               hh_e35b==1 ~ hh_e35a * hh_e38 * hh_e39 * hh_e40,
                               .default = NA_real_)
  ) # very rough calculation 
frq(mainwage$hh_e37b)
mainwage <- mainwage |> 
  mutate(in_kind=case_when(hh_e37b==5 ~ hh_e37a * hh_e38,
                           hh_e37b==8 ~ hh_e37a,
                           hh_e37b==4 ~ hh_e37a * hh_e38 * hh_e39 / 2, # total worked weeks divided by 2
                           hh_e37b==3 ~ hh_e37a * hh_e38 * hh_e39, 
                           hh_e37b==2 ~ hh_e37a * hh_e38 * hh_e39 * hh_e40/8,
                           hh_e37b==1 ~ hh_e37a * hh_e38 * hh_e39 * hh_e40,
                           .default = NA_real_)
  )
summary(mainwage$main_income)
summary(mainwage$in_kind)
# This data is likely to have enumerator or respondent errors. Unfortunately, we cannot know how and where the 
# error could be. For, now we will take the data as it is and think further during the model specification.
mainwage <- mainwage |>  
  mutate(income=rowSums(across(c(main_income,in_kind)), na.rm=TRUE)) |> 
  select(y5_hhid, income)

# secondwage

look_for(wage, details = "none")
secondwage <- wage[, c(2, 21:30)]
secondwage <- secondwage |> filter(hh_e49==1)
look_for(secondwage, details = "none")
frq(secondwage$hh_e55) # 2 say have a contract 
frq(secondwage$hh_e56) # these also do not seem like a contract but casual work.
frq(secondwage$hh_e51b)
secondwage <- secondwage |>
  mutate(
    hh_e51a = as.numeric(hh_e51a),   # cash wage amount
    hh_e51b = as.numeric(hh_e51b),   # cash wage time unit
    hh_e53a = as.numeric(hh_e53a),   # in-kind wage amount
    hh_e53b = as.numeric(hh_e53b),   # in-kind wage time unit
    hh_e54   = as.numeric(hh_e54))    # hours worked in last 7 days  
frq(secondwage$hh_e51b)
frq(secondwage$hh_e52)
secondwage <- secondwage |>  
  mutate(income=rowSums(across(c(hh_e51a,hh_e53a)), na.rm=TRUE)) |> 
  select(y5_hhid, income)

# very rough calculation : For the second wage, only hours worked in the last week 
# were recorded—there’s no info on total months or weeks worked during the year. 
# Since the main wage income is estimated on an annual basis and the second wage income 
# is calculated on a weekly basis. Annualizing second wage is not possible because the duration of the work is not clear.
# Also the reporting periods are different, months, days etc
# 
# Therefore, we will only consider the latest payment received. 
wage <- rbind(mainwage, secondwage)
remove(mainwage, secondwage)
wage <- collap(wage, income~y5_hhid, FUN=fsum)
tza5 <- left_join(tza5, wage, by=c("hhid"="y5_hhid"))
tza5 <- tza5 |> 
  tidyr::replace_na(list(income = 0)) |> 
  rename(wage_income=income)
var_label(tza5$wage_income) <- "income from main and temporary off-farm work"
remove(wage)

## Enterprise income

enterprise <- read_dta("data/tza/tza_wave5_2020_21/hh_sec_n.dta") |> 
  select(y5_hhid, hh_n19, hh_n20 )
look_for(enterprise)
enterprise <- enterprise |> 
  filter(!is.na(hh_n19)) |> 
  mutate(profit=hh_n19*hh_n20)
enterprise <- enterprise |> 
  group_by(y5_hhid) |> 
  summarise(enterprise_income=sum(profit)) |> 
  ungroup()
tza5 <- left_join(tza5, enterprise, by=c("hhid"="y5_hhid"))
tza5$enterprise_income[is.na(tza5$enterprise_income)] <- 0
var_label(tza5$enterprise_income) <- "annual enterprise income"
rm(enterprise)

##  Extension, advisory, credit----
extension <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_12a.dta") |>
  select(y5_hhid, sourceid, ag12a_01_1:ag12a_01_8, ag12a_02) |>
  filter(ag12a_02 == 1)               #receiving *any* advice

extension <- extension |>
  mutate(sourceid = as.numeric(sourceid)) |>
  mutate(
    extension_service = if_else(
      sourceid %in% c(1, 2, 3) &   #1-govt extension, 2-NGO, 3-Cooperative/ farmer's association 
        (ag12a_01_1 == 1 | ag12a_01_2 == 1 | ag12a_01_3 == 1 |
           ag12a_01_4 == 1 | ag12a_01_5 == 1 | ag12a_01_6 == 1 |
           ag12a_01_7 == 1 | ag12a_01_8 == 1),  1, 0 ))
extension <- collap(extension, extension_service~y5_hhid, FUN = fmax)
frq(extension$extension_service)
tza5 <- left_join(tza5, extension, by=c("hhid"="y5_hhid"))
tza5$extension_service[is.na(tza5$extension_service)] <- 0
var_label(tza5$extension_service) <- "received extension from institutions" 
remove(extension)

### Credit ----
credit <- read_dta("data/tza/tza_wave5_2020_21/hh_sec_p.dta") |> 
  select(y5_hhid, loanid, hh_p03)

credit <- credit |>
  mutate(
    credit_type = case_when(
      hh_p03 %in% c(1, 2, 3, 4, 5) ~ "Formal",
      hh_p03 %in% c(6, 7, 8, 9, 10, 11, 12, 13) ~ "Non-formal",
      is.na(hh_p03) ~ "No credit accessed"
    )
  )

var_label(credit$credit_type) <- "Type of credit received by household"

frq(credit$loanid)
frq(credit$credit_type)

## Note on credit_type:
# * Formal credit = from institutions such as commercial banks (1), MFIs (2), 
#   building societies (3), insurance companies (4), and others (5).
# * Non-formal credit = from sources like friends (6), local merchants (7), 
#   money lenders (8), employers (9), religious groups (10), NGOs (11), self-help groups (12), and others (13).
# * No credit accessed = households that did not report receiving any credit.

# Keep one row per household, taking the first credit_type encountered
credit <- credit |>
  distinct(y5_hhid, credit_type)

tza5 <- left_join(tza5, credit, by = c("hhid" = "y5_hhid"))

remove(credit)


## TLU ----
## page 10: https://appec-h.com/wp-content/uploads/2023/02/Livestock-production-systems.pdf
## Large ruminants = 0.7 TLU, small ruminants = 0.1, pigs = 0.2, poultry = 0.01, horses = 0.8,
## mules=  0.7, camels= 1
tlu <- read_dta("data/tza/tza_wave5_2020_21/lf_sec_02.dta") |> 
  select(y5_hhid, lvstckid, lf02_01, lf02_03, lf02_04_1, lf02_04_2) |> 
  filter(lf02_01==1)   |>
  mutate(lf02_04 = rowSums(across(c(lf02_04_1, lf02_04_2)), na.rm = TRUE))
frq(tlu$lvstckid)

tlu <- tlu |> 
  mutate(tlu=case_when(lvstckid %in% c(1:6) ~ lf02_04 * 0.7,
                       lvstckid %in% c(7:8) ~ lf02_04 * 0.1,
                       lvstckid==9 ~ lf02_04 * 0.2,
                       lvstckid %in% c(10:12) ~ lf02_04 * 0.01,
                       lvstckid %in% c(14) ~ lf02_04 * 0.7,
                       .default = 0))  #rabbits(13), dogs(15, cats916) and others (17) were omitted, since I couldn't find the TLU values for them.
tlu <- collap(tlu, tlu ~ y5_hhid, FUN = fsum)
tza5 <- left_join(tza5, tlu, by=c("hhid"="y5_hhid"))
remove(tlu)
tza5$tlu[is.na(tza5$tlu)] <- 0

## Land tenure ----
tenure <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_3a.dta") |>
  select(y5_hhid, plot_id, ag3a_25) |>
  rename(acquisition_mode = ag3a_25) |>
  mutate(plot_id = paste(y5_hhid, plot_id, sep = "-")) |>
  filter(!is.na(acquisition_mode))

tenure <- semi_join(tenure, maizeplots, by = "plot_id")  # KEPT ONLY MAIZE PLOTS
look_for(tenure)

# Create binary tenure_security: 1 = secure (inherited/gift/allocated/purchased), 0 = not secure (otherwise)
tenure <- tenure |>
  mutate(
    tenure_security = case_when(
      acquisition_mode %in% c(1, 2, 4, 5) ~ 1,
      !is.na(acquisition_mode) ~ 0,
      TRUE ~ NA_real_ ) )

var_label(tenure$tenure_security) <- "purchased, inherited, gifted, or allocated land ownership"

# Quick sanity check
tenure_diff_security <- tenure |>
  group_by(y5_hhid) |>
  summarise(
    unique_tenure = n_distinct(tenure_security, na.rm = TRUE),
    plots_count = n()
  ) |>
  filter(unique_tenure > 1) |> 
  ungroup()

print(tenure_diff_security)

tenure_diff_security <- tenure_diff_security |>
  mutate(diff = plots_count - unique_tenure)

frq(tenure_diff_security$diff)
frq(tenure_diff_security$plots_count)  # most have two plots

### NOTE:
# Some households have both secure and non-secure plots. 
# Combining tenure security only at the household level might not capture this complexity.
# The indicator should reflect if at least one maize plot is secure (purchased/inherited/allocated).
## sanity check end ##

tenure <- collap(tenure, tenure_security ~ y5_hhid, FUN = fmax)

tza5 <- left_join(tza5, tenure, by = c("hhid" = "y5_hhid"))

remove(tenure, tenure_diff_security)

## Input coupon ---- 
## In wave 5, if they received voucher for inputs. Instead their was a question why they used input,
## and one answer option is voucher. But this is not a good measure. 
inputcoupon <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_3a.dta") |> 
  select(y5_hhid, plot_id, ag3a_47, ag3a_52, ag3a_60, ag3a_64, ag3a_65a, ag3a_65d )

look_for(inputcoupon)  

inputcoupon <- inputcoupon |> 
  mutate(
    fert_voucher = if_else(ag3a_47 == 1 & !is.na(ag3a_52) & ag3a_52 == 2, 1, 0),
    herb_voucher = if_else(ag3a_60 == 1 & !is.na(ag3a_64) & ag3a_64 == 2, 1, 0),
    pest_voucher = if_else(ag3a_65a == 1 & !is.na(ag3a_65d) & ag3a_65d == 2, 1, 0)
    )

frq(inputcoupon$fert_voucher)
frq(inputcoupon$herb_voucher)
frq(inputcoupon$pest_voucher) 
## No farmer using fertilizer/herbicide/pesticide reported 'received voucher' 
#(code 2) as reason; no other input coupon variables available either.
remove(inputcoupon)

## Location ----
location <- read_dta("data/tza/tza_wave5_2020_21/hh_sec_a.dta") |> 
  select(y5_hhid, hh_a01_1, hh_a10, tracking_class) |> 
  rename(Region = hh_a01_1)
look_for(location)
frq(location$hh_a10)
frq(location$tracking_class)
location <- location |> 
  rename(originalhh=hh_a10)
frq(location$tracking_class) # still some more 
frq(location$originalhh)
n_distinct(location$y5_hhid)
tza5 <- left_join(tza5, location, by = c("hhid" = "y5_hhid")) 
rm(location)
frq(tza5$Region)
tza5 <- tza5 |> 
  mutate(zone = case_when(
    Region %in% c(4, 5, 6, 7, 51, 52, 53, 54, 55) ~ "Eastern",
    Region %in% c(2, 3, 21) ~ "Northern",
    Region %in% c(1, 13, 14) ~ "Central",
    Region %in% c(17, 18, 19, 20, 24, 25) ~ "Lake",
    Region %in% c(15, 16, 23) ~ "Western",
    Region %in% c(10, 11, 12, 22, 26) ~ "Southern Highlands",
    Region %in% c(8, 9) ~ "Southern",
    TRUE ~ NA_character_))
tza5$Region <- NULL
frq(tza5$zone)

## Other crops produced -----
plot_data <- read_dta("data/tza/tza_wave5_2020_21/ag_sec_4a.dta") |>  # plot level
  select(y5_hhid, plot_id, cropid)  
look_for(plot_data)
plot_data <- semi_join(plot_data, maizehh, by=c("y5_hhid"="hh_id_merge"))
plot_data <- plot_data |> 
  rowwise() |> 
  mutate(
    other_crops_count = sum(
      !is.na(c_across(c(cropid))) & 
        !(c_across(cropid) %in% c(11, 31:37, 43, 47))  # not maize or legumes
    )
  ) %>%
  ungroup()
plot_data <- plot_data |> 
  group_by(y5_hhid) |> 
  summarise(other_crops=sum(other_crops_count)) |> 
  ungroup()
tza5 <- left_join(tza5, plot_data, by=c("hhid"="y5_hhid"))
frq(tza5$other_crops)
var_label(tza5$other_crops) <- "number of other crops produced"
rm(maizehh, plot_data)
tza5$plot_dist_household <- NULL
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

remove(legume_then_maize_hhs, LR_legume_hhs, SR_maize_hhs,
       legume_codes, n_LR_legume_and_SR_maize_hhs, n_LR_legume_hhs)
