# mlw 3 ----
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               labelled,
               collapse)
getwd()
options(scipen = 999)

# identify panel households
hh16 <- read_dta("data/mlw/hh_mod_a_filt_16.dta") |> 
  select(y3_hhid, y2_hhid, ea_id,) |> 
  distinct(y3_hhid, .keep_all = TRUE) |> 
  rename(hhid=y3_hhid)

### ### ### ### ### ### ### ###
# HP data ----
### ### ### ### ### ### ### ###
hpdata <- read_dta("data/hp/Plot_dataset.dta") |>
  filter(country=="Malawi", wave==3) 
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
  select(hh_id_merge, hh_id_obs, plot_id_merge, plot_id_obs, parcel_id_obs, parcel_id_merge, plot_area_GPS, main_crop,
         farm_size, manager_id_merge, manager_id_obs, age_manager, female_manager, formal_education_manager, 
         primary_education_manager, respondent_id_obs, respondent_id_merge, improved,organic_fertilizer, 
         inorganic_fertilizer, soil_fertility_index, plot_dist_household, ag_asset_index, plot_certificate, 
         drought_shock, rain_shock,flood_shock,irrigated,  nb_seasonal_crop, used_pesticides, yield_value_LCU, 
         yield_value_USD) |>
  mutate(plot_id=plot_id_merge)
look_for(hpdata) # many missing

### ### ### ### ### ### ### ###
# Maize - legume intercropping----
### ### ### ### ### ### ### ###
### Garden means parcel; Garden -> Plot
garden <- read_dta("data/mlw/ag_mod_d_16.dta") |> # plot level 
  select(y3_hhid, gardenid, plotid, ag_d14, ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e, ag_d20_oth) |> 
  mutate(plot_id = paste(y3_hhid, gardenid, plotid, sep = "-"))
look_for(garden)
garden <- semi_join(garden, maizeplots, by="plot_id")
table(garden$ag_d14) # many plots are rented out or fallow
# We only keep those plots that are cultivated, even if these plots have been listed as maize fields in HP data.   
garden <- garden |> filter(ag_d14==1)
garden <- garden |> 
  group_by(y3_hhid)|>
  mutate(count_plot=n())|>
  ungroup()
val_labels(garden$ag_d20a)
table(garden$ag_d20a) # in rare cases other corps are listed first, 
# but the kept obs are those with maize as the highest value crop
table(garden$ag_d20_oth)  # 38 cowpeas
garden <- garden |> 
  mutate(intercrop=as.integer(
    if_any(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e, ag_d20_oth), ~ . %in% c(11, 12, 27, 34, 35, 36, 46, "COWPEAS"))
  ))

# Also in HP data, keep only plots that have been cultivated. 
plots <- garden |> select(plot_id)
maizeplots <- semi_join(maizeplots, plots, by="plot_id")
maizehh <- maizeplots |>  distinct(hh_id_merge)
hpdata <- semi_join(hpdata, maizeplots, by="plot_id")
remove(plots)

### ### ### ### ### ### ### ###
# Maize - legume rotation----
### ### ### ### ### ### ### ###
modd <- read_dta("data/mlw/ag_mod_d_16.dta") |> 
  select(y3_hhid, gardenid, plotid, ag_d69, ag_d69_oth, ag_d70, ag_d70_oth) |> 
  mutate(plot_id = paste(y3_hhid, gardenid, plotid, sep = "-")) |> 
  select( -y3_hhid,-gardenid, -plotid)
modd <- semi_join(modd, maizeplots, by="plot_id")
table(modd$ag_d70_oth) # 16 COWPEAS 
modd <- modd |> 
  mutate(rotation=as.integer(
    if_any(c(ag_d69, ag_d69_oth, ag_d70, ag_d70_oth), ~ . %in% c(11, 12, 27, 34, 35, 36, 46, "COWPEAS"))
  ))
look_for(garden)
plot3 <- left_join(garden, modd, by="plot_id") |> 
  select(-ag_d20a, -ag_d20b, -ag_d20c, -ag_d20d, -ag_d20e, -ag_d20_oth, -ag_d69, -ag_d69_oth, -ag_d70, -ag_d70_oth)
remove(garden,modd)
var_label(plot3$intercrop) <- "legume intercropped maize plot"
var_label(plot3$rotation) <- "legume is cultivated before or after"
var_label(plot3$count_plot) <- "number of main maize plots"
### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###
modd <- read_dta("data/mlw/ag_mod_d_16.dta") |> 
  select(y3_hhid, gardenid, plotid, ag_d01) |>
  mutate(plot_id = paste(y3_hhid, gardenid, plotid, sep = "-")) |> 
  rename(id_code=ag_d01)
modd <- semi_join(modd, maizeplots, by="plot_id")
individual <- read_dta("data/mlw/hh_mod_b_16.dta") |> 
  select(y3_hhid, id_code, hh_b03, hh_b05a) |> 
  mutate(female=ifelse(hh_b03==2,1,0))
modd <- left_join(modd,individual, by=c("y3_hhid", "id_code"))
remove(individual)
var_label(modd$female) <- "female primary dm concern. activities on maize plots" # also on crops to be planted
modd <- modd |> select(plot_id, female,hh_b05a )
plot3 <- left_join(plot3, modd, by="plot_id")
plot3 <- plot3 |> rename(age_manager_row=hh_b05a)
remove(modd)

### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
# total consumption is absent if MLW
hdds3 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Malawi", wave==3) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, hh_asset_index, HDDS, share_kg_sold)
hdds3 <- hdds3|> 
  semi_join(maizehh, by="hh_id_merge")
look_for(hdds3) # 297 missing for share_kg_sold
remove(maizehh)

### ### ### ### ### ### ### ###
# Other variables---- 
### ### ### ### ### ### ### ###
community <- read_dta("data/mlw/com_cd_16.dta") |> 
  select(ea_id, com_cd18a , com_cd18b)
table(community$com_cd18b)
val_labels(community$com_cd18b) # 1 meter 2 km
community <- community |> 
  mutate(market_distance=case_when(com_cd18b==1 ~ com_cd18a/1000,
                                   .default = com_cd18a)) |> 
  select(ea_id, market_distance)
community %>%
  count(ea_id) %>%
  filter(n > 1) # one copy 
community <- community |> distinct(ea_id,.keep_all = TRUE)

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###
plot3 <- left_join(plot3, hpdata, by="plot_id")
remove(hpdata)
look_for(plot3)
plot3 <- plot3 |> rename(hhid=y3_hhid)
plot3 <- left_join(plot3, hh16, by="hhid")
plot3 <- plot3 |> relocate(ea_id,.after = hhid)
plot3 <- left_join(plot3, community, by="ea_id")
remove(community, maizeplots)
table(plot3$female_manager, plot3$female) # same information.
plot3 <- plot3 |> select(-female)
# manager age is missing for 102 plots.
# market_distance still missing for 892 observations. No data on market distance can be used. 
plot3 <- plot3 |> select(-market_distance)

## Collapse / aggregate ----
mlw3 <- plot3 |> 
  select(-ag_d14, -hh_id_merge,-plot_id_merge, -plot_id_obs, -parcel_id_merge, -parcel_id_obs,
         -respondent_id_obs, -respondent_id_merge,-age_manager_row)
look_for(mlw3)
wave3mean <- collap(mlw3,
                    farm_size + age_manager + plot_dist_household + ag_asset_index + count_plot + 
                    soil_fertility_index  ~ hhid,
                    FUN = fmean,
                    na.rm = TRUE)
wave3sum <- collap(mlw3,
                   plot_area_GPS +nb_seasonal_crop +yield_value_LCU + yield_value_USD  ~ hhid,
                   FUN = fsum,
                   na.rm = TRUE)
aggregated_vars <- c("farm_size", "age_manager", "plot_dist_household", "soil_fertility_index",
                     "ag_asset_index", "count_plot", "plot_area_GPS", "nb_seasonal_crop", 
                     "yield_value_LCU", "yield_value_USD")
mlw3 <- mlw3 %>% select(-all_of(aggregated_vars))
look_for(mlw3)
mlw3 <- mlw3 |> 
  select(1, 2, 6:8, 11:22)
look_for(mlw3)
# For all remaining binary variables, I will select the max. Meaning, for example,
# at least one plot is managed by female, at least one maize plot manager has primary education, 
# at least one plot is being intercropped and etc. 
wave3max <- collap(mlw3,
                   ~ hhid, 
                   FUN = fmax,
                   na.rm = TRUE)
mlw3 <- left_join(wave3mean, wave3sum, by=join_by("hhid"))
mlw3 <- left_join(mlw3, wave3max, by=join_by("hhid"))
mlw3 <- left_join(mlw3, hdds3, by=c("hhid"="hh_id_merge"))
remove(wave3max, wave3sum,wave3mean,aggregated_vars, hdds3, plot3)
mlw3 <- mlw3 |> mutate(wave="wave3")
mlw3 <- mlw3 |> 
  relocate(hh_id_obs, .after = hhid) |> 
  relocate(ea_id, .after = hh_id_obs)

## Cleaning missing data ----
look_for(mlw3)
## share_kg_sold in HP data has many missing; reason: set to missing if reported share above 100% percent or
##  if household does not harvest any crops.
##  age is missing for many observations in the raw data. 83 missing for manager age. 
missing <- is.na(mlw3$share_kg_sold)|
  is.na(mlw3$age_manager)
mlw3 <- subset(mlw3, subset= !missing)
var_label(mlw3$plot_area_GPS) <- "Area (ha) of maize plots"
var_label(mlw3$nb_seasonal_crop) <- "Number of seasonal crops grown on maize plots"
remove(missing)



## Notes on missing data ----
## 1023 hhs before any cleaning of missing data
## 83 missing for manager age
## 41 missing household size, dependency ratio, consumption and some other hh level variables
## 95 hhs do not have agricultural assets but still have asset index. This is becuase, pca (factor) analysis still includes those with 
## no asset and gives them a lower score. 













