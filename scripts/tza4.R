# tza 4 ----

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
  filter(country == "Tanzania", wave == 4)

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
# Four variables — soil_fertility_index, plot_dist_household, rain_shock, and flood_shock — have substantial missing data, each with 1,631 missing values.

### ### ### ### ### ### ### ###
# Maize - legume intercropping---- 
### ### ### ### ### ### ### ###
intercropdata <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4a.dta") |>  # crops by plot
  select("y4_hhid", "plotnum", "zaocode", "zaoname", "ag4a_04") |> 
  rename(intercrop = ag4a_04) |> 
  filter(!is.na(zaocode))

intercropdata <- intercropdata |> 
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-"))

intercropdata <- semi_join(intercropdata, maizeplots, by = "plot_id")
n_distinct(intercropdata$plot_id)  # 1204 distinct plots

intercropdata <- intercropdata |> 
  filter(zaocode %in% c(11, 31, 32, 33, 34, 35, 36, 37, 43, 47))
intercropdata <- intercropdata |> 
  group_by(y4_hhid)|>
  mutate(count_plot=n())|>
  ungroup()
# plots have a definition that is akin to parcels, and no parcels are tracked. Thus, even if a plot has three crops, it would not mean
# intercropping. Thus, we must identify plots where both maize and legumes have been reported to be intercropped. 
# In this case, we can believe that maize and legume are intercropped.

legume_codes <- c(31, 32, 33, 34, 35, 36, 37, 43, 47)

intercropdata <- intercropdata |> 
  group_by(plot_id) |> 
  mutate(
    has_maize = any(zaocode == 11 & intercrop == 1),
    has_legume = any(zaocode %in% legume_codes & intercrop == 1),
    intercrop_legume = as.integer(has_maize & has_legume)
  ) |> 
  ungroup()

look_for(intercropdata)

intercropdata <- collap(intercropdata, intercrop_legume + count_plot ~ plot_id, FUN = fmax)
frq(intercropdata$intercrop_legume)  # 434 maize plots out of 1204 are intercropped with legumes.

# in HP data, keep only plots from the long panel (extended refresh sample not included) 
maizeplots <- semi_join(maizeplots, intercropdata, by = "plot_id")
maizehh <- maizeplots |> distinct(hh_id_merge)  # 911 maize HHs in wave 4.
hpdata <- semi_join(hpdata, maizeplots, by = "plot_id")

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
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-"))

# Identify legume plots in the short rainy season
SRseason <- SRseason |> 
  filter(zaocode %in% legume_codes) |> 
  select(y4_hhid, plotnum, zaocode) |> 
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-"))

# Merge the maize plots to ensure they are from households with maize as high-value crop
LRseason <- semi_join(LRseason, maizeplots, by = "plot_id")
SRseason <- semi_join(SRseason, maizeplots, by = "plot_id")

SRseason <- SRseason |> 
  mutate(rotation = 1) |> 
  select(plot_id, rotation)

LRseason |> group_by(plot_id) |> 
  filter(n() > 1) |> 
  ungroup()

SRseason |> group_by(plot_id) |> 
  filter(n() > 1) |> 
  ungroup()  # several duplicates - intercropped. 

SRseason <- SRseason |> 
  distinct(plot_id, .keep_all = TRUE)

LRseason <- LRseason |> 
  select(plot_id)

# Merge maize plots from the long rainy season with legume plots from the short rainy season 
LRseason <- left_join(LRseason, SRseason, by = "plot_id")
frq(LRseason$rotation)  # 37 distinct plots with maize-legume rotation across 33 households
LRseason$rotation[is.na(LRseason$rotation)] <- 0

plot4 <- left_join(intercropdata, LRseason, by = "plot_id")
remove(LRseason, SRseason, legume_codes, intercropdata)

## improved maize  ----
## here we don't have the info at parcel level but only at hh level. all seeds were asked. We concentrate on maize. 
seed <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4a.dta") |> 
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-")) |> 
  select(y4_hhid, plot_id, zaocode, ag4a_06, ag4a_08) |> 
  filter(zaocode==11)
look_for(seed)
seed |> group_by(y4_hhid) |> 
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
plot4 <- left_join(plot4, seed, by="plot_id")
frq(plot4$improved_maize)
remove(seed)

### ### ### ### ### ### ### ###
# Soil management ----
# crop residue short season
plots <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4b.dta") |> 
  select(y4_hhid, plotnum, zaocode) |> 
  filter(!is.na(zaocode)) |> 
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-")) |> 
  select(-plotnum)
look_for(plots)
plots <- plots |> 
  distinct(y4_hhid, zaocode, .keep_all = TRUE)
soilman <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_5b.dta") |> # crop level no plot key 
  select(y4_hhid, ag5b_33, zaocode)
look_for(soilman)
frq(soilman$ag5b_33)
soilman <- soilman |>
  filter(zaocode %in% c(11, 31, 32, 33, 34, 35, 36, 37, 43, 47)) # keep only maize and legume
soilman <- soilman |>
  filter(!is.na(ag5b_33)) |> 
  mutate(crop_residue=if_else(ag5b_33==2, 1, 0)) |> 
  select(-ag5b_33)
soilman <- left_join(soilman, plots, by=c("y4_hhid", "zaocode"))
soilman <- soilman |> 
  group_by(plot_id) |> 
  summarise(crop_residue_short=max(crop_residue), .groups = "drop")
plot4 <- left_join(plot4, soilman, by="plot_id")
plot4$crop_residue_short[is.na(plot4$crop_residue_short)] <- 0
var_label(plot4$crop_residue_short) <- "Residues from short rainy season were left in the field"

# residue long season
plots <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4a.dta") |> 
  select(y4_hhid, plotnum, zaocode) |> 
  filter(!is.na(zaocode)) |> 
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-")) |> 
  select(-plotnum)
# for long season we concentrate on maize and legumes to possible be able to link to the fields of interest as there is no direct key
plots <- plots |>
  filter(zaocode %in% c(11, 31, 32, 33, 34, 35, 36, 37, 43, 47))
plots <- plots |> 
  distinct(y4_hhid, zaocode, .keep_all = TRUE)
soilman <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_5a.dta") |> # crop level no plot key 
  select(y4_hhid, zaocode, ag5a_33)
soilman <- soilman |>
  filter(zaocode %in% c(11, 31, 32, 33, 34, 35, 36, 37, 43, 47))
look_for(soilman)
soilman <- soilman |>
  filter(!is.na(ag5a_33)) |> 
  mutate(crop_residue=if_else(ag5a_33==2, 1, 0)
  )
soilman <- left_join(soilman, plots, by=c("y4_hhid", "zaocode"))
soilman <- soilman |> 
  group_by(plot_id) |> 
  summarise(crop_residue_long=max(crop_residue),.groups = "drop") |> 
  ungroup()
plot4 <- left_join(plot4, soilman, by="plot_id")
plot4$crop_residue_long[is.na(plot4$crop_residue_long)] <- 0
var_label(plot4$crop_residue_long) <- "Residues from the latest long season were left in the field"
look_for(plot4)
rm(soilman, plots)

## Soil Type ----
soil_type <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_3a.dta") |>  
  select(y4_hhid, plotnum, ag3a_10, ag3a_11, ag3a_12) |>
  rename(soil_type = ag3a_10) |>  
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-"))
soil_type <- semi_join(soil_type, maizeplots, by = "plot_id")
look_for(soil_type)  # No missing values in soil_type
soil_type |>
  count(y4_hhid) |>
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
plot4 <- left_join(plot4, soil_type, by="plot_id")
var_label(plot4$soil) <-  "loam soil type"
remove(soil_type)

### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###
## DM at plot level ----
modd <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_3a.dta") |> 
  select(y4_hhid, plotnum, ag3a_08_1) |>
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-")) |> 
  rename(id_code=ag3a_08_1) |> 
  filter(!is.na(id_code))
modd <- semi_join(modd, maizeplots, by="plot_id")

individual <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_b.dta") |> 
  select(y4_hhid, indidy4, hh_b02, hh_b04) |> 
  mutate(female=ifelse(hh_b02==2,1,0)) |> 
  rename(id_code = indidy4) # Rename 'indidy4' to 'id_code' in the 'individual' dataset
modd <- left_join(modd, individual, by=c("y4_hhid", "id_code"))
summary(modd)
var_label(modd$female) <- "female primary dm concern. crops selection" # on crops to be planted

modd <- modd |> select(plot_id, female, hh_b04 )  
plot4 <- left_join(plot4, modd, by="plot_id")
plot4 <- plot4 |> rename(age_manager_row=hh_b04)
remove(modd)

## DM on crop income ----
sales <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_5a.dta") |> 
  select(y4_hhid, ag5a_10_1, ag5a_10_2) |> 
  filter(ag5a_10_1==1)
look_for(sales)
sales <- semi_join(sales, maizehh, by=c("y4_hhid"="hh_id_merge"))
sales <- sales |> 
  rename(id_code=ag5a_10_1)
individual <- individual |> 
  select(y4_hhid, id_code, female)
sales <- left_join(sales,individual, by=c("y4_hhid", "id_code"))
look_for(sales)
sales$id_code <- NULL
sales <- sales |> 
  rename(id_code=ag5a_10_2) # now second decision-maker
sales <- left_join(sales,individual, by=c("y4_hhid", "id_code"))
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
n_distinct(sales$y4_hhid)
sales <- collap(sales, female_dm ~ y4_hhid, FUN = fmax)
sales <- sales |> 
  mutate(female_dm= factor(female_dm, levels = c(1,2,3), labels = c("Female", "Joint", "Male")))
frq(sales$female_dm)
rm(individual)


## Asset ownership ---- 
## no data available on owners - there is a section on household assets, but no info on ownership.

## Education ----
## variable of interest: if HH has an educated adult female: at least completed the primary school. 

# Load individual data with sex, age, relationship to head
individual <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_b.dta") |> 
  select(y4_hhid, indidy4, hh_b02, hh_b04, hh_b05) |> 
  mutate(female = ifelse(hh_b02 == 2, 1, 0)) |> 
  filter(female == 1) |>
  filter(hh_b04 > 17) |>
  rename(id_code = indidy4)
# Merge with education data (highest education level completed)
edu <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_c.dta") |> 
  select(y4_hhid, indidy4, hh_c07) |> 
  rename(id_code = indidy4)
individual <- left_join(individual, edu, by = c("y4_hhid", "id_code"))
remove(edu)
# Keep only maize-growing households
individual <- semi_join(individual, maizehh, by = c("y4_hhid" = "hh_id_merge"))
frq(individual$hh_c07)  # D7 - Class 7, D4- Class 4, F4-Form 4
look_for(individual)

individual$hh_c07 <- as.numeric(individual$hh_c07)
# Collapse to HH level – keep max female education in HH
individual <- collap(individual, hh_c07 ~ y4_hhid, FUN = fmax, na.rm = TRUE)
# We will focus on primary education:primary edu is 7 years in TZ; so >16
individual <- individual |> 
  mutate(female_edu=ifelse(hh_c07 > 16,1,0)) |> # =1 if hh has educated female with at least primary education completed. 
  filter(!is.na(hh_c07)) |> 
  select(y4_hhid, female_edu)

### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
hdds4 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Tanzania", wave==4) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, hh_asset_index, HDDS, share_kg_sold)
hdds4 <- hdds4|> 
  semi_join(maizehh, by="hh_id_merge")

# FCS
fcs <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_j3.dta")
look_for(fcs)
fcs$itemcode <- as.factor(fcs$itemcode)
fcs <- fcs |> 
  group_by(y4_hhid) %>%
  mutate(
    sum_A_B = sum(hh_j08_3[itemcode %in% c(1, 2)], na.rm = TRUE),
    sum_A_B = case_when(sum_A_B > 7 ~ 7,
                        .default = sum_A_B),
    count = case_when(itemcode== 1 ~ sum_A_B,
                      .default = hh_j08_3)
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
  group_by(y4_hhid) |> 
  summarise(fcs=sum(total)) |> 
  ungroup()
fcs <- semi_join(fcs, maizehh, by=c("y4_hhid" = "hh_id_merge"))
hdds4 <- left_join(hdds4, fcs, by=c("hh_id_merge" = "y4_hhid"))
summary(hdds4$fcs)
rm(fcs)

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###
plot4 <- left_join(plot4, hpdata, by = "plot_id")
look_for(plot4)
remove(hpdata)
plot4 <- plot4 |> rename(hhid=hh_id_merge)
table(plot4$female_manager, plot4$female) # minor differences
mismatched_rows <- plot4 |> filter(female_manager != female)
View(mismatched_rows) 

# --------------------------DOUBT--------------------------------------------- #
# Checking consistency between 'female_manager' and 'female' after merging
# Found 8 mismatches:
# - 1 case where female_manager = 0 but female = 1 - would mean that male is the manager but female decided the crop
# - 7 cases where female_manager = 1 but female = 0 ; female is the manager but male decided the crop.
# Both are kept! 
# ---------------------------------------------------------------------------- #
remove(mismatched_rows)
look_for(plot4)
# manager age is missing for 2 plots.

## Collapse / aggregate ----
tza4 <- plot4 |> 
  select(-plot_id_merge, -respondent_id_obs, -respondent_id_merge,-age_manager_row)
look_for(tza4)
tza4 <- tza4 |> select(10:11, 1:9, 13, 15:36)
n_distinct(tza4$hh_id_obs) # 911 hhs, 1204 obs
tza4 <- tza4 |> 
  group_by(hh_id_obs) |> 
  mutate(plot_area=sum(plot_area_GPS, na.rm = TRUE),
         yield_value_USD=sum(yield_value_USD, na.rm = TRUE),
         yield_value_LCU=sum(yield_value_LCU, na.rm = TRUE)) |> 
  ungroup() |> 
  relocate(plot_area, .after = plot_area_GPS)
# control if hh used any organic or inorganic fertilizer in intercrop field or any other field.
table(tza4$rotation, tza4$intercrop_legume) # intercroppping more common than rotation
tza4 <- tza4 |> 
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
n_distinct(tza4$plot_id) # 1204 distinct plots as obs count 
table(tza4$intercrop, tza4$rotation) # 747 non adopted plots
tza4adoption <- tza4 |>
  relocate(intercrop, .after = rotation) |> 
  mutate(keep=if_else(intercrop==1 & rotation==1, 1, 0))
# keep only adopting households; for non-adopters will select the biggest parcel and manager characteristics
tza4adoption <- tza4adoption |> 
  group_by(hh_id_obs) |> 
  filter(intercrop != 0 | rotation != 0) |> 
  ungroup()
table(tza4adoption$intercrop, tza4adoption$rotation)
# there are only 33 observations where both rotation and intercrop are observed.
# first keep these plots and hhs, and drop if there is another plot where one practice is practiced
tza4adoption <- tza4adoption |> 
  group_by(hh_id_obs) |> 
  mutate(keephh=max(keep)) |> 
  ungroup()
tza4adoption <- tza4adoption |>
  filter((keep==0 & keephh==0) | (keep==1 & keephh==1)) 
tza4adoption$keephh <- NULL
tza4adoption <- tza4adoption |> 
  group_by(hh_id_obs) |> 
  filter(intercrop==max(intercrop, na.rm = TRUE )) |>  # there were two parcels that had been rotated but not intercropped within hh
  ungroup()
tza4adoption <- tza4adoption |>
  mutate(rotation=rotation_check) # for the indication of adoption
n_distinct(tza4adoption$hh_id_obs) # 382 hhs
# keep the biggest plot
tza4adoption <- tza4adoption |> 
  group_by(hh_id_obs) |> 
  filter(plot_area_GPS == max(plot_area_GPS, na.rm = TRUE)) |> 
  ungroup()
n_distinct(tza4adoption$hh_id_obs) # one more household with two parcels same size
tza4adoption <- tza4adoption |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> 
  relocate(dupl, .after=hh_id_obs) |>  ungroup()
# no difference
tza4adoption <- tza4adoption |>
  distinct(hh_id_obs, .keep_all = TRUE)

# select the biggest plot among non-adopters
tza4 <- tza4 |> 
  filter(intercrop == 0 & rotation == 0) 
tza4 <- tza4 |> 
  group_by(hh_id_obs) |> 
  filter(plot_area_GPS == max(plot_area_GPS, na.rm = TRUE)) |> 
  ungroup()
n_distinct(tza4$hh_id_obs) # 599 hh 601 obs
tza4 <- tza4 |> distinct() # 
tza4 <- tza4 |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs)

# trying complete cases to see if any hh will be dropped at the end. if this results in dropping a hh,
# approach is changed later
tza4 <- tza4 |> # seelcting neares plot, differences are often meager
  group_by(hh_id_obs) |>  
  filter(
    (dupl > 1 & nb_seasonal_crop==max(nb_seasonal_crop, na.rm=TRUE)) |
      # if hh has no duplicates → keep all rows
      (dupl == 1)
  ) |> 
  ungroup()
tza4 <- tza4 |> select(-dupl) |>
  group_by(hh_id_obs) |> mutate(dupl=n()) |> 
  relocate(dupl, .after=hhid) |>  ungroup() 
# no difference in the remaining one duplicate 
tza4 <- tza4 |> select(-dupl) |> distinct(hh_id_obs, .keep_all = TRUE) 
# Combine adopter and non-adopter hhs
tza4 <- bind_rows(tza4, tza4adoption)
n_distinct(tza4$hh_id_obs) # 911 hhs and 981 obs
tza4 <- tza4 |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs) |> 
  arrange(desc(dupl), hh_id_obs)
# There are hhs, which have adopter plots and non-adopted plots. keep only adopted plots
tza4 <- tza4 |> 
  group_by(hh_id_obs) |> 
  filter((dupl>1 & (intercrop==1 | rotation==1)) |
           dupl==1) |> ungroup()
tza4 <- tza4 |> 
  select(-dupl, -rotation_check, -keep) |> 
  arrange(hh_id_obs)
look_for(tza4) 
var_label(tza4$plot_area) <- "Area (in hectares) of all maize plots"
labs <- var_label(plot4)
common_names <- intersect(names(tza4), names(labs))
var_label(tza4)[common_names] <- labs[common_names]
look_for(tza4) 
rm(labs, tza4adoption, common_names)
tza4$plot_area_GPS <- NULL
tza4$improved_maize <- NULL
look_for(tza4)
var_label(tza4$rotation) <- "HH rotated at least one maize field with legumes during the previous short season"
var_label(tza4$count_plot) <- "number of main maize plots"
var_label(tza4$iv_maize) <- "HH has cultivated improved maize in at least one field"
var_label(tza4$organic_fertilizer) <- "organig fertilizer applied to at least one maize plot"
var_label(tza4$inorganic_fertilizer) <- "inorganig fertilizer applied to at least one maize plot"
tza4<- left_join(tza4, sales, by=c("hhid"="y4_hhid"))
frq(tza4$female_dm) 
var_label(tza4$female_dm) <- "Who kept/decided what to do with crop income?"
tza4 <- left_join(tza4, hdds4, by=c("hhid"="hh_id_merge"))
tza4<- left_join(tza4, individual, by=c("hhid"="y4_hhid"))
tza4 <- tza4 |> 
  mutate(female_edu=if_else(is.na(female_edu), 0, female_edu))
remove(hdds4, plot4, individual)
tza4 <- tza4 |> mutate(wave="wave4")


## Cleaning missing data ----
look_for(tza4)
### share_kg_sold----
# Subsistence orientation
# Identify households with a subsistence orientation (did not sell any maize in 2014)
subsistence_ag <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_5a.dta") |> 
  select(hh_id_merge = y4_hhid, ag5a_01) |> 
  semi_join(maizehh, by = "hh_id_merge") |> 
  mutate(subsistence_ag=if_else(ag5a_01==2,1,0)) |> # sell_produce == 2
  rename(hhid=hh_id_merge)
frq(subsistence_ag$subsistence_ag)   # Only 1 HH has a missing value
subsistence_ag <- collap(subsistence_ag, subsistence_ag~hhid, FUN = fmin, na.rm = TRUE) # min captures if any crop was sold
frq(subsistence_ag$subsistence_ag) # 332 are subsistence oriented
tza4 <- left_join(tza4, subsistence_ag, by="hhid")
frq(tza4$subsistence_ag) # 1 still missing. no harvest? 
remove(subsistence_ag)


### ### ### ### ### ### ### ###
# Other variables  ----
### ### ### ### ### ### ### ###

### ### ### ### ### ### ### ###
## Distance from Plot to Home, Road and Market ====

distance_from_plot <- "data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_3a.dta"

# ───── Plot-to-home distance ─────
plot_hh_dist <- read_dta(distance_from_plot) |>  
  select(y4_hhid, plotnum, ag3a_02_1) |>  
  rename(plot_hh_distance = ag3a_02_1) |>  
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-")) |>  
  semi_join(maizeplots, by = "plot_id")
attr(plot_hh_dist$plot_hh_distance, "label") <- "Distance from plot to home (in KMs), self-reported"
look_for(plot_hh_dist)
plot_hh_dist |>
  count(y4_hhid) |>
  filter(n > 1)
plot_hh_dist<- plot_hh_dist |>
  group_by(y4_hhid) |>
  summarise(plot_hh_dist_mean = mean(plot_hh_distance, na.rm = TRUE)) |> 
  ungroup()
tza4 <- left_join(tza4, plot_hh_dist, by = c("hhid" = "y4_hhid"))

# ───── Plot-to-road distance ─────
plot_road_dist <- read_dta(distance_from_plot) |>  
  select(y4_hhid, plotnum, ag3a_02_2) |>  
  rename(plot_road_distance = ag3a_02_2) |>  
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-")) |>  
  semi_join(maizeplots, by = "plot_id")
attr(plot_road_dist$plot_road_distance, "label") <- "Distance from plot to road (in KMs), self-reported"
look_for(plot_road_dist)
plot_road_dist |>
  count(y4_hhid) |>
  filter(n > 1)
plot_road_dist<- plot_road_dist |>
  group_by(y4_hhid) |>
  summarise(plot_road_dist_mean = mean(plot_road_distance, na.rm = TRUE)) |>
  ungroup()
tza4 <- left_join(tza4, plot_road_dist, by = c("hhid" = "y4_hhid"))

# ───── Plot-to-market distance ─────
plot_market_dist <- read_dta(distance_from_plot) |>  
  select(y4_hhid, plotnum, ag3a_02_3) |>  
  rename(plot_market_distance = ag3a_02_3) |>  
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-")) |>  
  semi_join(maizeplots, by = "plot_id")
attr(plot_market_dist$plot_market_distance, "label") <- "Distance from plot to market (in KMs), self-reported"
look_for(plot_market_dist)
plot_market_dist |>
  count(y4_hhid) |>
  filter(n > 1)
plot_market_dist<- plot_market_dist |>
  group_by(y4_hhid) |>
  summarise(plot_market_dist_mean = mean(plot_market_distance, na.rm = TRUE)) |> 
  ungroup()
tza4 <- left_join(tza4, plot_market_dist, by = c("hhid" = "y4_hhid"))
remove(plot_hh_dist, plot_market_dist, plot_road_dist, distance_from_plot)

## Non-farm income ----
wage <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_e.dta")
look_for(wage, details = "none")
wage <- wage[, c(1:3, 5:7, 9:10, 28, 35:45, 51, 57:64, 65:66)]
wage <- semi_join(wage, maizehh, by = c("y4_hhid" = "hh_id_merge"))
look_for(wage, details = "none")
mainwage <- wage[, c(2, 7, 10:20)]
mainwage <- mainwage |> filter(hh_e24==1)
look_for(mainwage, details = "none")
frq(mainwage$hh_e26_2)
mainwage <- mainwage |>
  mutate(
    hh_e26_1 = as.numeric(hh_e26_1),   # cash wage amount
    hh_e26_2 = as.numeric(hh_e26_2),   # cash wage time unit
    hh_e28_1 = as.numeric(hh_e28_1),   # in-kind wage amount
    hh_e28_2 = as.numeric(hh_e28_2),   # in-kind wage time unit
    hh_e29   = as.numeric(hh_e29),     # months worked
    hh_e30   = as.numeric(hh_e30),     # weeks per month
    hh_e31   = as.numeric(hh_e31)      # hours per week
  )
frq(mainwage$hh_e26_2)

# If the individual's last payment covers last month and , let's say he worked for 4 months during past year,
# we can assume his yearly income is last month payment amount multiplied by 4 (months worked). Conversion to
# hourly rates is not necessary if the payment covers a different period than a day or hour. The difference
# between mlw and tzn is that in mlw, individual could say that the last payment covered for example past two 
# months or weeks or etc, but in tnz it is just a month - no amount but just unit question - hh_e26_2. 
# Moreover, for example if the individual's last payment was reported for a day and he worked 2 month, 2 weeks
# each month and 40 hours per week. His total hours worked would be 2*2*40/8 expressed in days. 

mainwage <- mainwage |> 
  mutate(main_income=case_when(hh_e26_2==5 ~ hh_e26_1 * hh_e29,
                               hh_e26_2==8 ~ hh_e26_1,
                               hh_e26_2==6 ~ hh_e26_1 * hh_e29/4, #some worked only three month but got paid for quarter?!
                               hh_e26_2==4 ~ hh_e26_1 * hh_e29 * hh_e30 / 2, # total worked weeks divided by 2
                               hh_e26_2==3 ~ hh_e26_1 * hh_e29 * hh_e30, 
                               hh_e26_2==2 ~ hh_e26_1 * hh_e29 * hh_e30 * hh_e31/8,
                               hh_e26_2==1 ~ hh_e26_1 * hh_e29 * hh_e30 * hh_e31,
                               .default = NA_real_)
         ) # very rough calculation 
frq(mainwage$hh_e28_2)
mainwage <- mainwage |> 
  mutate(in_kind=case_when(hh_e28_2==5 ~ hh_e28_1 * hh_e29,
                           hh_e28_2==8 ~ hh_e28_1,
                           hh_e28_2==6 ~ hh_e28_1 * hh_e29/4,
                           hh_e28_2==4 ~ hh_e28_1 * hh_e29 * hh_e30 / 2, # total worked weeks divided by 2
                           hh_e28_2==3 ~ hh_e28_1 * hh_e29 * hh_e30, 
                           hh_e28_2==2 ~ hh_e28_1 * hh_e29 * hh_e30 * hh_e31/8,
                           .default = NA_real_)
         )
summary(mainwage$main_income)
summary(mainwage$in_kind)
# This data is likely to have enumerator or respondent errors. Unfortunately, we cannot know how and where the 
# error could be. For, now we will take the data as it is and think further during the model specification.
mainwage <- mainwage |>  
  mutate(income=rowSums(across(c(main_income,in_kind)), na.rm=TRUE)) |> 
  select(y4_hhid, income)

#secondwage

look_for(wage, details = "none")
secondwage <- wage[, c(2, 7, 21:31)]
secondwage <- secondwage |> filter(hh_e42==1)
look_for(secondwage, details = "none")
frq(secondwage$hh_e52) # 4 say have a contract 
frq(secondwage$hh_e52a) # these also do not seem like a contract but casual work.
frq(secondwage$hh_e44_2)
secondwage <- secondwage |>
  mutate(
    hh_e44_1 = as.numeric(hh_e44_1),   # cash wage amount
    hh_e44_2 = as.numeric(hh_e44_2),   # cash wage time unit
    hh_e46_1 = as.numeric(hh_e46_1),   # in-kind wage amount
    hh_e46_2 = as.numeric(hh_e46_2),   # in-kind wage time unit
    hh_e50   = as.numeric(hh_e50))    # hours worked in last 7 days  
frq(secondwage$hh_e44_2)
frq(secondwage$hh_e45)
secondwage <- secondwage |>  
  mutate(income=rowSums(across(c(hh_e44_1,hh_e46_1)), na.rm=TRUE)) |> 
  select(y4_hhid, income)

# very rough calculation : For the second wage, only hours worked in the last week 
# were recorded—there’s no info on total months or weeks worked during the year. 
# Since the main wage income is estimated on an annual basis and the second wage income 
# is calculated on a weekly basis. Annualizing second wage is not possible because the duration of the work is not clear.
# Also the reporting periods are different, months, days etc
# 
# Therefore, we will only consider the latest payment received. 
wage <- rbind(mainwage, secondwage)
remove(mainwage, secondwage)
wage <- collap(wage, income~y4_hhid, FUN=fsum)
tza4 <- left_join(tza4, wage, by=c("hhid"="y4_hhid"))
tza4 <- tza4 |> 
  tidyr::replace_na(list(income = 0)) |> 
  rename(wage_income=income)
var_label(tza4$wage_income) <- "income from main and temporary off-farm work"
remove(wage)

## Enterprise income

enterprise <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_n.dta") |> 
  select(y4_hhid, hh_n19, hh_n20 )
look_for(enterprise)
enterprise <- enterprise |> 
  filter(!is.na(hh_n19)) |> 
  mutate(profit=hh_n19*hh_n20)
enterprise <- enterprise |> 
  group_by(y4_hhid) |> 
  summarise(enterprise_income=sum(profit)) |> 
  ungroup()
tza4 <- left_join(tza4, enterprise, by=c("hhid"="y4_hhid"))
tza4$enterprise_income[is.na(tza4$enterprise_income)] <- 0
var_label(tza4$enterprise_income) <- "annual enterprise income"
rm(enterprise)

##  Extension, advisory, credit----

extension <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_12a.dta") |>
  select(y4_hhid, sourceid, ag12a_01_1:ag12a_01_8, ag12a_02) |>
  filter(ag12a_02 == 1)               #receiving *any* advice

extension <- extension |>
  mutate(sourceid = as.numeric(sourceid)) |>
  mutate(
    extension_service = if_else(
      sourceid %in% c(1, 2, 3) &   #1-govt extension, 2-NGO, 3-Cooperative/ farmer's association 
        (ag12a_01_1 == 1 | ag12a_01_2 == 1 | ag12a_01_3 == 1 |
           ag12a_01_4 == 1 | ag12a_01_5 == 1 | ag12a_01_6 == 1 |
           ag12a_01_7 == 1 | ag12a_01_8 == 1),  1, 0 ))
extension <- collap(extension, extension_service~y4_hhid, FUN = fmax)
frq(extension$extension_service)
tza4 <- left_join(tza4, extension, by=c("hhid"="y4_hhid"))
tza4$extension_service[is.na(tza4$extension_service)] <- 0
var_label(tza4$extension_service) <- "received extension from institutions" 
remove(extension)

### Credit ----
credit <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_p.dta") %>% 
  select(y4_hhid, loancode, hh_p03)

credit <- credit |>
  mutate(
    credit_type = case_when(
      hh_p03 %in% c(1, 2, 3, 4, 5) ~ "Formal",
      hh_p03 %in% c(6, 7, 8, 9, 10, 11, 12, 13) ~ "Non-formal",
      is.na(hh_p03) ~ "No credit accessed"
    )
  )

var_label(credit$credit_type) <- "Type of credit received by household"

frq(credit$loancode)
frq(credit$credit_type)

## Note on credit_type:
# * Formal credit = from institutions such as commercial banks (1), MFIs (2), 
#   building societies (3), insurance companies (4), and others (5).
# * Non-formal credit = from sources like friends (6), local merchants (7), 
#   money lenders (8), employers (9), religious groups (10), NGOs (11), self-help groups (12), and others (13).
# * No credit accessed = households that did not report receiving any credit.

# Keep one row per household, taking the first credit_type encountered
credit <- credit |>
  distinct(y4_hhid, credit_type)

tza4 <- left_join(tza4, credit, by = c("hhid" = "y4_hhid"))

remove(credit)


## TLU ----
## page 10: https://appec-h.com/wp-content/uploads/2023/02/Livestock-production-systems.pdf
## Large ruminants = 0.7 TLU, small ruminants = 0.1, pigs = 0.2, poultry = 0.01, horses = 0.8,
## mules=  0.7, camels= 1
tlu <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/lf_sec_02.dta") |> 
  select(y4_hhid, lvstckid, lf02_01, lf02_03, lf02_04_1, lf02_04_2) |> 
  filter(lf02_01==1)   |>
  mutate(lf02_04 = rowSums(across(c(lf02_04_1, lf02_04_2)), na.rm = TRUE))
frq(tlu$lvstckid)
tlu <- tlu |> 
  mutate(tlu=case_when(lvstckid %in% c(1:6) ~ lf02_04 * 0.7,
                       lvstckid %in% c(7:8) ~ lf02_04 * 0.1,
                       lvstckid==9 ~ lf02_04 * 0.2,
                       lvstckid %in% c(10:12) ~ lf02_04 * 0.01,
                       lvstckid %in% c(14) ~ lf02_04 * 0.7,
                       .default = 0)) 
tlu <- collap(tlu, tlu ~ y4_hhid, FUN = fsum)
tza4 <- left_join(tza4, tlu, by=c("hhid"="y4_hhid"))
remove(tlu)
frq(tza4$tlu)
tza4$tlu[is.na(tza4$tlu)] <- 0

## Land tenure ----
tenure <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_3a.dta") |>
  select(y4_hhid, plotnum, ag3a_25) |>
  rename(acquisition_mode = ag3a_25) |>
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-")) |>
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
  group_by(y4_hhid) |>
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

tenure <- collap(tenure, tenure_security ~ y4_hhid, FUN = fmax)

tza4 <- left_join(tza4, tenure, by = c("hhid" = "y4_hhid"))

remove(tenure, tenure_diff_security)

## Input coupon & improved variety use---- 
fertcoupon <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_3a.dta") |> 
  select(y4_hhid, plotnum, ag3a_50, ag3a_57)
look_for(fertcoupon)
fertcoupon <- fertcoupon |> 
  mutate(ag3a_50=as.numeric(ag3a_50),
         ag3a_57=as.numeric(ag3a_57),
         fert_voucher = if_else(ag3a_50 == 1 | ag3a_57 == 1, 1, 0))
frq(fertcoupon$fert_voucher)
seedcoupon<- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4a.dta") |> 
  select(y4_hhid, zaocode, plotnum, ag4a_06, ag4a_08, ag4a_13a)
table(seedcoupon$zaocode, seedcoupon$ag4a_13a) # 11 maize but no legume coupon.
seedcoupon <- seedcoupon |> 
  mutate(ag4a_13a=as.numeric(ag4a_13a),
         seed_voucher = if_else(ag4a_13a == 1, 1, 0))
look_for(seedcoupon)
frq(seedcoupon$seed_voucher)
coupon <- left_join(
  fertcoupon |> select(y4_hhid, plotnum, fert_voucher),
  seedcoupon |> select(y4_hhid, plotnum, ag4a_06, ag4a_08, seed_voucher),
  by = c("y4_hhid", "plotnum"))
coupon <- coupon |>
  rename(mainseed_variety_name = ag4a_06,
         seed_type = ag4a_08 ) |>
  mutate(seed_type=as.numeric(seed_type)) # 3 is also improved but recycled.
coupon <- collap(coupon, seed_voucher + fert_voucher ~ y4_hhid, FUN = fmax, na.rm = TRUE)
tza4 <- left_join(tza4, coupon, by=c("hhid"="y4_hhid"))
tza4$seed_voucher[is.na(tza4$seed_voucher)] <- 0
tza4$fert_voucher[is.na(tza4$fert_voucher)] <- 0
frq(tza4$seed_voucher)
frq(tza4$fert_voucher)
var_label(tza4$seed_voucher) <- "HH had obtained voucher for seed"
var_label(tza4$fert_voucher) <- "HH had obtained voucher for inorganic fertilizer"
remove(coupon, seedcoupon, fertcoupon)

## Location ----
location <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_a.dta") |> 
  select(y4_hhid, hh_a01_1) |> 
  rename(Region = hh_a01_1)
tza4 <- left_join(tza4, location, by = c("hhid" = "y4_hhid")) 
frq(tza4$Region)
tza4 <- tza4 |> 
  mutate(zone = case_when(
    Region %in% c(4, 5, 6, 7, 51, 52, 53, 54, 55) ~ "Eastern",
    Region %in% c(2, 3, 21) ~ "Northern",
    Region %in% c(1, 13, 14) ~ "Central",
    Region %in% c(17, 18, 19, 20, 24, 25) ~ "Lake",
    Region %in% c(15, 16, 23) ~ "Western",
    Region %in% c(10, 11, 12, 22, 26) ~ "Southern Highlands",
    Region %in% c(8, 9) ~ "Southern",
    TRUE ~ NA_character_))
tza4$Region <- NULL
frq(tza4$zone)
rm(location)


## Other crops produced -----
plot_data <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4a.dta") |>  # plot level
  select(y4_hhid, plotnum, zaocode, plotname, zaoname)  
look_for(plot_data)
plot_data <- semi_join(plot_data, maizehh, by=c("y4_hhid"="hh_id_merge"))
plot_data <- plot_data |> 
  rowwise() |> 
  mutate(
    other_crops_count = sum(
      !is.na(c_across(c(zaocode))) & 
        !(c_across(zaocode) %in% c(11, 31:37, 43, 47))  # not maize or legumes
    )
  ) %>%
  ungroup()
plot_data <- plot_data |> 
  filter(!is.na(zaocode)) |> 
  group_by(y4_hhid) |> 
  summarise(other_crops=sum(other_crops_count)) |> 
  ungroup()
tza4 <- left_join(tza4, plot_data, by=c("hhid"="y4_hhid"))
frq(tza4$other_crops)
var_label(tza4$other_crops) <- "number of other crops produced"
rm(maizehh, plot_data)

## Slop ----
modd <-  read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_3a.dta") |> 
  select(y4_hhid, plotnum, plotname, ag3a_17) |>
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-"))
modd <- semi_join(modd, maizeplots, by="plot_id")
frq(modd$ag3a_17) # do mode max again, if there is a steep slope that will be captured this way too. 
modd <- modd |>
  mutate(ag3a_17 = as.numeric(modd$ag3a_17))

mode_or_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  
  tab <- table(x)
  most_common <- which(tab == max(tab))
  
  if (length(most_common) == 1) {
    return(as.numeric(names(most_common)))
  } else {
    return(max(as.numeric(names(most_common))))
  }
} 

modd <- collap(modd, ag3a_17 ~ y4_hhid, FUN =mode_or_max) 
frq(modd$ag3a_17)
modd <- modd |> 
  mutate(flat_slope= case_when(ag3a_17 %in% c(1,2) ~ 1,
                               .default = 0)) |> 
  select(y4_hhid, flat_slope)
frq(modd$flat_slope)
tza4 <- left_join(tza4, modd, by=c("hhid"="y4_hhid"))
var_label(tza4$flat_slope) <- "Dominant slope"
remove(modd, maizeplots, mode_or_max)
tza4$plot_dist_household <- NULL

### ### ### ### ### ### ### ### #### ##### #### ### ###
# Auxiliary Check: How Many LR Legume-Growing Households Planted Maize in the SR
### ### ### ### ### ### ### ###### ### ###  ### ### ### 

legume_codes <- c(31, 32, 33, 34, 35, 36, 37, 43, 47)

# Households that grew legumes in the Long Rainy Season
LR_legume_hhs <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4a.dta") |> 
  filter(zaocode %in% legume_codes) |> 
  distinct(y4_hhid)

# Households that grew maize in the Short Rainy Season
SR_maize_hhs <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4b.dta") |> 
  filter(zaocode == 11) |> 
  distinct(y4_hhid)

# How many legume-growing HHs in LR also grew maize in SR?
legume_then_maize_hhs <- inner_join(LR_legume_hhs, SR_maize_hhs, by = "y4_hhid")

n_LR_legume_hhs <- nrow(LR_legume_hhs)
n_LR_legume_and_SR_maize_hhs <- nrow(legume_then_maize_hhs)

cat("Total households that grew legumes in LR:", n_LR_legume_hhs, "\n")
cat("Of them, households that also grew maize in SR:", n_LR_legume_and_SR_maize_hhs, "\n")

# Out of 981 households that grew legumes in the Long Rainy Season (LR),
# 155 also grew maize in the Short Rainy Season (SR)
# Maybe this suggests that most of these households were likely primarily legume growers,
# with limited transition to maize in SR.

remove(legume_then_maize_hhs, LR_legume_hhs, SR_maize_hhs,
   legume_codes, n_LR_legume_and_SR_maize_hhs, n_LR_legume_hhs)

