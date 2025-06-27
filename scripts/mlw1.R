# mlw 1 ----
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
  filter(country=="Malawi", wave==1) 
# We concentrate on the plots where maize is the main crop. 
maizeplots <- hpdata |> 
  filter(main_crop=="MAIZE") |> 
  select(hh_id_merge, parcel_id_merge, plot_id_merge) |> 
  mutate(plot_id=plot_id_merge)
maizehh <- maizeplots |>  distinct(hh_id_merge)
hpdata|>
  filter(main_crop=="MAIZE") |> 
  group_by(hh_id_obs, parcel_id_obs, plot_id_obs)|>
  filter(n() > 1)|>
  ungroup() # 0 duplicates - plots are uniquely identified  
hpdata <- hpdata|>
  filter(main_crop=="MAIZE") |> 
  select(hh_id_merge, hh_id_obs, ea_id_merge, plot_id_merge, plot_id_obs, parcel_id_obs, parcel_id_merge, plot_area_GPS, main_crop,
         farm_size, manager_id_merge, manager_id_obs, age_manager, female_manager, formal_education_manager, 
         primary_education_manager, respondent_id_obs, respondent_id_merge, improved,organic_fertilizer, 
         inorganic_fertilizer, soil_fertility_index, elevation, plot_dist_household, ag_asset_index, plot_certificate, 
         drought_shock, rain_shock, flood_shock, irrigated, nb_seasonal_crop, used_pesticides, yield_value_LCU, 
         yield_value_USD) |>
  mutate(plot_id=plot_id_merge)
look_for(hpdata) # many missing, especially improved seeds

### ### ### ### ### ### ### ###
# Maize - legume intercropping----
### ### ### ### ### ### ### ###
### Garden means parcel; Garden -> Plot
garden <- read_dta("data/mlw/ag_mod_d_10.dta") |>   # plot level 
  select(case_id, ag_d00, ag_d14, ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e) |> 
  mutate(plot_id = paste(case_id, ag_d00, sep = "-"))
look_for(garden, details = "none")
garden <- semi_join(garden, maizeplots, by="plot_id")
frq(garden$ag_d14) # some are fallow and missing
# We only keep those plots that are cultivated, even if these plots have been listed as maize fields in HP data.   
garden <- garden |> filter(ag_d14==1)
frq(garden$ag_d20a)
garden <- garden |> 
  group_by(case_id)|>
  mutate(count_plot=n())|>
  ungroup()
val_labels(garden$ag_d20a)
table(garden$ag_d20a) # in very rare cases other crops are listed first, 
# but the kept obs are those with maize as the highest value crop
garden <- garden |> 
  mutate(intercrop=as.integer(
    if_any(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e), ~ . %in% c(11:16, 27, 34:36, 46))
  ),
  hybrid_maize=as.integer(
    if_any(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e), ~ . %in% c(3,4))
  ))

# Also in HP data, keep only plots that have been cultivated. 
plots <- garden |> select(plot_id)
maizeplots <- semi_join(maizeplots, plots, by="plot_id")
maizehh <- maizeplots |>  distinct(hh_id_merge)
hpdata <- semi_join(hpdata, maizeplots, by="plot_id")
remove(plots)

### ### ### ### ### ### ### ###
# Maize - legume rotation----
# If legume was cultivated after the rainy season
### ### ### ### ### ### ### ###
modm <- read_dta("data/mlw/ag_mod_m_10.dta") |> 
  select(case_id, ag_m0b, ag_m0d) |> 
  filter(!is.na(ag_m0d))
# plot id is with D's (dry) but with R's in the rainy season
library(stringr)
modm <- modm |> 
  mutate(ag_m0b = str_replace(ag_m0b, "^D", "R")) |> 
  mutate(plot_id = paste(case_id, ag_m0b, sep = "-"))
val_labels(modm$ag_m0d)
modm <- modm |> 
  mutate(rotation=case_when(ag_m0d %in% c(11:16, 27, 34:36, 46) ~ 1,
                            .default = 0)) |> 
  filter(rotation==1) # only 24 observations 
modm |> 
  group_by(plot_id)|>
  filter(n() > 1)|>
  ungroup() # no duplicate
modm <- modm |> select(plot_id, rotation)
garden <- left_join(garden, modm, by="plot_id")
frq(garden$rotation) # 20 plots only.
garden$rotation[is.na(garden$rotation)] <- 0
plot1 <- garden |> 
  select(case_id, plot_id, count_plot, intercrop, rotation, hybrid_maize)
remove(garden,modm)
var_label(plot1$intercrop) <- "=1 if legume intercropped maize plot"
var_label(plot1$rotation) <- "=1 if legume is cultivated after" # ONLY AFTER CAN BE IDENTIFIED
var_label(plot1$count_plot) <- "number of main maize plots"
var_label(plot1$hybrid_maize) <- " =1 if hybrid maize was cultivated"

### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###
## DM at plot level ----
modd <-  read_dta("data/mlw/ag_mod_d_10.dta") |>   # plot level 
  select(case_id, ag_d00, ag_d01) |> 
  mutate(plot_id = paste(case_id, ag_d00, sep = "-")) |> 
  rename(id_code=ag_d01)
modd <- semi_join(modd, maizeplots, by="plot_id")
individual <- read_dta("data/mlw/hh_mod_b_10.dta") |> 
  select(case_id, id_code, hh_b01, hh_b03, hh_b05a) |> 
  mutate(female=ifelse(hh_b03==2,1,0))
modd <- left_join(modd, individual, by=c("case_id", "id_code"))
remove(individual)
var_label(modd$female) <- "female primary dm concern. activities on maize plots" # also on crops to be planted
modd <- modd |> select(plot_id, female, hh_b05a)
plot1 <- left_join(plot1, modd, by="plot_id")
plot1 <- plot1 |> rename(age_manager_row=hh_b05a)
remove(modd)

## Asset ownership ---- 
## no data available on owners

## Education ----
## variable of interest: if HH has an educated adult female
sex <- read_dta("data/mlw/hh_mod_b_10.dta") |> 
  select(case_id, id_code, hh_b03, hh_b05a)
individual <- read_dta("data/mlw/hh_mod_c_10.dta") |> 
  select(case_id, id_code, hh_c05a, hh_c05b, hh_c09)
individual <- left_join(individual, sex, by=c("case_id", "id_code"))
individual <- individual |> 
  mutate(female=ifelse(hh_b03==2,1,0)) |> 
  filter(female==1) |> 
  filter(hh_b05a>17)
individual <- semi_join(individual, maizehh, by=c("case_id" = "hh_id_merge"))
frq(individual$hh_c09) # pscl - primary school; junior certificate of edu; Malawi school certificate
look_for(individual)
individual$hh_c09 <- as.numeric(individual$hh_c09)
individual <- collap(individual, hh_c09 ~ case_id, FUN = fmax, na.rm = TRUE)
frq(individual$hh_c09)
individual <- individual |> 
  mutate(female_edu=ifelse(hh_c09==1,0,1)) |> 
  select(-hh_c09)
frq(individual$female_edu)
remove(sex)

### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
# total consumption is absent if MLW
hdds1 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Malawi", wave==1) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, hh_asset_index, HDDS, share_kg_sold)
hdds1 <- hdds1|> 
  semi_join(maizehh, by="hh_id_merge")
look_for(hdds1) # 124 missing for share_kg_sold

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###
plot1 <- left_join(plot1, hpdata, by="plot_id")
remove(hpdata)
look_for(plot1)
plot1 <- plot1 |> rename(hhid=case_id)
plot1 <- plot1 |> relocate(ea_id_merge,.after = hhid)
table(plot1$female_manager, plot1$female) # same information.
plot1 <- plot1 |> select(-female)
# manager age is missing for 13 plots like in hpdata. can be dropped. 

## Collapse / aggregate ----
mlw1 <- plot1 |> 
  select(-hh_id_merge,-plot_id_merge, -plot_id_obs, -parcel_id_merge, -parcel_id_obs,
         -respondent_id_obs, -respondent_id_merge,-age_manager_row)
look_for(mlw1)
mlw1basics <- mlw1 |> 
  select(hhid, hh_id_obs, ea_id_merge) |> 
  distinct()
wave1mean <- collap(mlw1,
                   farm_size + age_manager + elevation + plot_dist_household + ag_asset_index + count_plot + 
                     soil_fertility_index  ~ hhid,
                   FUN = fmean,
                   na.rm = TRUE)
wave1sum <- collap(mlw1,
                   plot_area_GPS + nb_seasonal_crop +yield_value_LCU + yield_value_USD  ~ hhid,
                   FUN = fsum,
                   na.rm = TRUE)
aggregated_vars <- c("farm_size", "age_manager", "elevation", "plot_dist_household", "soil_fertility_index",
                     "ag_asset_index", "count_plot", "plot_area_GPS", "nb_seasonal_crop", 
                     "yield_value_LCU", "yield_value_USD")
mlw1 <- mlw1 %>% select(-all_of(aggregated_vars))
look_for(mlw1)
mlw1 <- mlw1 |> 
  select(1, 4:6, 11:22)
look_for(mlw1)
# For all remaining binary variables, I will select the max. Meaning, for example,
# at least one plot is managed by female, at least one maize plot manager has primary education, 
# at least one plot is being intercropped and etc. 
mlw1 <- mlw1 |> 
  mutate(across(c(female_manager, formal_education_manager, primary_education_manager,
                  inorganic_fertilizer, plot_certificate, irrigated),
                as.numeric))
wave1max <- collap(mlw1,
                   ~ hhid, 
                   FUN = fmax,
                   na.rm = TRUE)
mlw1 <- left_join(mlw1basics, wave1mean, by=join_by("hhid"))
mlw1 <- left_join(mlw1, wave1sum, by=join_by("hhid"))
mlw1 <- left_join(mlw1, wave1max, by=join_by("hhid"))
mlw1 <- left_join(mlw1, hdds1, by=c("hhid"="hh_id_merge"))
mlw1 <- left_join(mlw1, individual, by=c("hhid"="case_id"))
mlw1 <- mlw1 |> 
  mutate(female_edu=if_else(is.na(female_edu), 0, female_edu))
remove(mlw1basics, wave1max, wave1sum, wave1mean, aggregated_vars, hdds1, plot1, individual)
mlw1 <- mlw1 |> mutate(wave="wave1")

## Cleaning missing data ----
look_for(mlw2)
### share_kg_sold----
### share_kg_sold in HP data has many missing; reason: set to missing if reported share above 100% percent or
##  if household does not harvest any crops.
##  We will capture subsistence orientation: HHs do not sell any crops. 
sales <- read_dta("data/mlw/ag_mod_i_10.dta") |> 
  select(case_id, ag_i0b, ag_i01)
sales <- semi_join(sales, maizehh, by=c("case_id"="hh_id_merge"))
look_for(sales, details = "basic")
frq(sales$ag_i01)
sales <- sales |> mutate(subsistence_ag=if_else(ag_i01==2,1,0))
sales <- collap(sales, subsistence_ag ~ case_id, FUN = fmin, na.rm = TRUE ) # if some consider for sale then not a subsistence. 
sales <- sales |> rename(hhid=case_id)
mlw1 <- left_join(mlw1, sales, by="hhid")
frq(mlw1$subsistence_ag)
remove(sales)

### ### ### ### ### ### ### ###
# Other variables   ----
### ### ### ### ### ### ### ###
## Distance ----
community <- read_dta("data/mlw/com_cd_10.dta") |> 
  select(ea_id, com_cd18a , com_cd18b)
frq(community$com_cd18b) # 1 meter 2 km 3 mile
community <- community |> 
  mutate(market_distance=case_when(com_cd18b==1 ~ com_cd18a/1000,
                                   com_cd18b==3 ~ com_cd18a/0.62,
                                   .default = com_cd18a)) |> 
  select(ea_id, market_distance)
community %>%
  count(ea_id) %>%
  filter(n > 1) # distinct 
community <- community |> distinct(ea_id,.keep_all = TRUE)




















































