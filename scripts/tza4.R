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

# identify panel households
hh14 <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/hh_filters.dta") |> 
  select(y4_hhid) |> 
  distinct(y4_hhid, .keep_all = TRUE) 

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
remove(individual)
var_label(modd$female) <- "female primary dm concern. crops selection" # on crops to be planted
modd <- modd |> select(plot_id, female, hh_b04 )  
plot4 <- left_join(plot4, modd, by="plot_id")
plot4 <- plot4 |> rename(age_manager_row=hh_b04)
remove(modd)

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

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###
plot4 <- left_join(plot4, hpdata, by = "plot_id")
look_for(plot4)
remove(hpdata)
plot4 <- plot4 |> rename(hhid=hh_id_merge)
table(plot4$female_manager, plot4$female)
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
  select(-plot_id_merge, -plot_id,  -respondent_id_obs, -respondent_id_merge,-age_manager_row)
look_for(tza4)
tza4basics <- tza4 |> 
  select(hhid, hh_id_obs) |> 
  distinct()
wave4mean <- collap(tza4,
                    farm_size + age_manager + plot_dist_household + ag_asset_index + count_plot + 
                      soil_fertility_index  ~ hhid,
                    FUN = fmean,
                    na.rm = TRUE)
wave4sum <- collap(tza4,
                   plot_area_GPS + nb_seasonal_crop +yield_value_LCU + yield_value_USD  ~ hhid,
                   FUN = fsum,
                   na.rm = TRUE)
aggregated_vars <- c("farm_size", "age_manager", "plot_dist_household", "soil_fertility_index", "count_plot",
                     "ag_asset_index", "plot_area_GPS", "nb_seasonal_crop", 
                     "yield_value_LCU", "yield_value_USD")
tza4 <- tza4 %>% select(-all_of(aggregated_vars))
look_for(tza4)

tza4 <- tza4 |> 
  select(4, 1:3, 10:21)
# ---------------------------------------------------------------------------- #

look_for(tza4)
# For all remaining binary variables, I will select the max. Meaning, for example,
# at least one plot is managed by female, at least one maize plot manager has primary education, 
# at least one plot is being intercropped and etc. 
tza4 <- tza4 |> 
  mutate(across(c(female_manager, formal_education_manager, primary_education_manager,
                  inorganic_fertilizer, plot_certificate, irrigated),
                as.numeric))
wave4max <- collap(tza4,
                   ~ hhid, 
                   FUN = fmax,
                   na.rm = TRUE)
tza4 <- left_join(tza4basics, wave4mean, by=join_by("hhid"))
tza4 <- left_join(tza4, wave4sum, by=join_by("hhid"))
tza4 <- left_join(tza4, wave4max, by=join_by("hhid"))
tza4 <- left_join(tza4, hdds4, by=c("hhid"="hh_id_merge"))
tza4<- left_join(tza4, individual, by=c("hhid"="y4_hhid"))
tza4 <- tza4 |> 
  mutate(female_edu=if_else(is.na(female_edu), 0, female_edu))
remove(tza4basics, wave4max, wave4sum, wave4mean, aggregated_vars, hdds4, plot4, individual)
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
# Distance from Plot to Home, Road and Market

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
  summarise(plot_hh_dist_mean = mean(plot_hh_distance, na.rm = TRUE))

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
  summarise(plot_road_dist_mean = mean(plot_road_distance, na.rm = TRUE))

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
  summarise(plot_market_dist_mean = mean(plot_market_distance, na.rm = TRUE))

tza4 <- left_join(tza4, plot_market_dist, by = c("hhid" = "y4_hhid"))

remove(plot_hh_dist, plot_market_dist, plot_road_dist)

## Non-farm income ----

wage <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_e.dta")
look_for(wage, details = "none")
wage <- wage[, c(1:3, 5:7, 9:10, 28, 35:45, 51, 57:64)]
wage <- semi_join(wage, maizehh, by = c("y4_hhid" = "hh_id_merge"))
look_for(wage, details = "none")
mainwage <- wage[, c(1:5, 7, 10:20)]
mainwage <- mainwage |> filter(!is.na(hh_e06_1))
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

# Convert cash payment to hourly rate
mainwage$hourly_rate_cash <- with(mainwage, case_when(
  hh_e26_2 == 1 ~ hh_e26_1 / 1,     # Hour
  hh_e26_2 == 2 ~ hh_e26_1 / 8,     # Day (8 hours)
  hh_e26_2 == 3 ~ hh_e26_1 / 40,    # Week (40 hours)
  hh_e26_2 == 4 ~ hh_e26_1 / 80,    # Fortnight (80 hours)
  hh_e26_2 == 5 ~ hh_e26_1 / 160,   # Month (160 hours)
  hh_e26_2 == 6 ~ hh_e26_1 / 480,   # Quarter (3 months)
  hh_e26_2 == 7 ~ hh_e26_1 / 960,   # Half-year
  hh_e26_2 == 8 ~ hh_e26_1 / 1920,  # Year (160*12)
  TRUE ~ NA_real_
))

# Calculate total_hours worked in the last 12 months
mainwage$total_hours <- with(mainwage, hh_e29 * hh_e30 * hh_e31)
# Calculate annual_cash_income
mainwage$annual_cash_income <- with(mainwage, hourly_rate_cash * total_hours)

# Calculate hourly_rate_inkind
mainwage$hourly_rate_inkind <- with(mainwage, case_when(
  hh_e28_2 == 1 ~ hh_e28_1 / 1,     
  hh_e28_2 == 2 ~ hh_e28_1 / 8,     
  hh_e28_2 == 3 ~ hh_e28_1 / 40,    
  hh_e28_2 == 4 ~ hh_e28_1 / 80,    
  hh_e28_2 == 5 ~ hh_e28_1 / 160,   
  hh_e28_2 == 6 ~ hh_e28_1 / 480,   
  hh_e28_2 == 7 ~ hh_e28_1 / 960,   
  hh_e28_2 == 8 ~ hh_e28_1 / 1920,  
  TRUE ~ NA_real_
))

# Calculate annual_inkind_income
mainwage$annual_inkind_income <- with(mainwage, hourly_rate_inkind * total_hours)

# Calculate total wage income (cash + in-kind)
mainwage$annual_total_income <- with(mainwage, annual_cash_income + annual_inkind_income)

summary(mainwage$hourly_rate_inkind)
summary(mainwage$annual_inkind_income)
summary(mainwage$annual_total_income)

##### Note: Extreme in-kind and total income values observed, likely due to calculation errors!? 
##### Secondary wage calculations & income from enterprises pending ###

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
credit <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_p.dta") |> 
  select(y4_hhid, loancode, hh_p03, hh_p10_1, hh_p10_2, hh_p10_3)
# Note: there is no variable to capture "no_credit_access"; only the details of 
# respondents who has a credit has been asked

frq(credit$loancode) 

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
                       .default = 0))  #hare(13)  and dogs(15) were omitted, since I couldn't find the TLU values for them.
tlu <- collap(tlu, tlu ~ y4_hhid, FUN = fsum)
tza4 <- left_join(tza4, tlu, by=c("hhid"="y4_hhid"))
remove(tlu)

## Land tenure - ----

tenure <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_3a.dta") |>  
  select(
    y4_hhid, plotnum,
    ag3a_25,    # How was the plot acquired?
    ag3a_28a,   # Type of legal certificate
    ag3a_28d,   # Other types of legal/ownership documentation
    ag3a_30,    # Right to sell
    ag3a_38a    # Land dispute
  ) |> 
  rename(land_dispute = ag3a_38a)
tenure <- tenure |> 
  filter(!is.na(land_dispute))
n_distinct(tenure$y4_hhid)
tenure <- collap(tenure, land_dispute ~ y4_hhid, FUN = fmax, na.rm = TRUE)

tenure <- left_join(maizehh, tenure, by = c("hh_id_merge" = "y4_hhid"))
tza4   <- left_join(tza4, tenure, by = c("hhid" = "hh_id_merge"))

tza4 <- tza4 |> 
  mutate(tenure_security = case_when(
    is.na(land_dispute) ~ 1,  # No reported dispute = secure
    TRUE ~ 0                 # Dispute reported = insecure
  )) |> 
  select(-land_dispute)

rm(tenure)

# NOTE: The 'land_dispute' variable only reflects disputes in the past 12 months, 
# which may not fully capture long-term tenure security. 
# Should we consider constructing a more robust indicator by incorporating 
# additional variables such as plot acquisition type (ag3a_25) and legal 
# documentation (ag3a_28a, ag3a_28d)?



## Soil Type ----

soil_type <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_3a.dta") |>  
  select(y4_hhid, plotnum, ag3a_10) |>  
  rename(soil_type = ag3a_10) |>  
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-"))
soil_type <- semi_join(soil_type, maizeplots, by = "plot_id")
look_for(soil_type)  # No missing values in soil_type

soil_type |>
  count(y4_hhid) |>
  filter(n > 1)

# Identify households with more than one soil type
multi_soil_hhids <- soil_type |>
  group_by(y4_hhid) |>
  summarise(unique_soil_types = n_distinct(soil_type)) |>
  filter(unique_soil_types > 1) |>
  pull(y4_hhid)

# View plots with multiple soil types within the same household
soil_type |>
  filter(y4_hhid %in% multi_soil_hhids) |>
  arrange(y4_hhid, plotnum)

# Note: There are 124 maize plots belonging to households with more than one soil type.
# How can we aggregate soil type information at the household level to merge it with tza4?


## Input coupon & improved variety use---- 
fertcoupon <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_3a.dta") |> 
  select(y4_hhid,plotnum,ag3a_50, ag3a_57)
look_for(fertcoupon)
fertcoupon <- fertcoupon |> 
  mutate(fert_voucher = if_else(ag3a_50 == 1 | ag3a_57 == 1, 1, 0))

seedcoupon<- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4a.dta") |> 
  select(y4_hhid,plotnum,ag4a_06,ag4a_08, ag4a_13a)
seedcoupon <- seedcoupon |> 
  mutate(seed_voucher = if_else(ag4a_13a == 1, 1, 0))
look_for(seedcoupon)

coupon <- left_join(
  fertcoupon |> select(y4_hhid, plotnum, fert_voucher),
  seedcoupon |> select(y4_hhid, plotnum, ag4a_06, ag4a_08, seed_voucher),
  by = c("y4_hhid", "plotnum"))

coupon <- coupon |>
  rename(mainseed_variety_name = ag4a_06,
    seed_type = ag4a_08 ) |>
  mutate(improved_seed = if_else(seed_type == 1, 1, 0))


coupon <- collap(coupon, seed_voucher + fert_voucher + improved_seed ~ y4_hhid, FUN = fmax, na.rm = TRUE)
tza4 <- left_join(tza4, coupon, by=c("hhid"="y4_hhid"))
tza4$seed_voucher[is.na(tza4$seed_voucher)] <- 0
tza4$fert_voucher[is.na(tza4$fert_voucher)] <- 0
frq(tza4$seed_voucher)
frq(tza4$fert_voucher)
var_label(tza4$seed_voucher) <- "HH had obtained voucher for seed"
var_label(tza4$fert_voucher) <- "HH had obtained voucher for inorganic fertilizer"
var_label(tza4$improved_seed) <- "The main seeds used in the HH are improved seeds"
remove(coupon, seedcoupon, fertcoupon)


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



