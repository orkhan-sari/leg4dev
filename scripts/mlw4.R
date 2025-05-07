# mlw 4 ----
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               labelled,
               collapse,
               sjmisc)
getwd()
options(scipen = 999)

# In 2019, 3,178 households stemmed from 2,368 of the 2016 households representing a household-level attrition rate of 5.6 percent.
# Individuals split from a household were followed. Who gest the hhid? - Check page 4 in HP https://hdl.handle.net/10986/42446 
# hh_id_obs from the HP data is the key for panel identifier later. 

### ### ### ### ### ### ### ###
# HP data ----
### ### ### ### ### ### ### ###
hpdata <- read_dta("data/hp/Plot_dataset.dta") |>
  filter(country=="Malawi", wave==4) 
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
         inorganic_fertilizer, soil_fertility_index, plot_dist_household, ag_asset_index, plot_certificate, 
         drought_shock, rain_shock,flood_shock,irrigated,  nb_seasonal_crop, used_pesticides, yield_value_LCU, 
         yield_value_USD) |>
  mutate(plot_id=plot_id_merge)
look_for(hpdata) # many missing

### ### ### ### ### ### ### ###
# Maize - legume intercropping----
### ### ### ### ### ### ### ###
### Garden means parcel; Garden -> Plot
garden <- read_dta("data/mlw/ag_mod_d_19.dta") |> # plot level 
  select(y4_hhid, gardenid, plotid, ag_d14, ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e, ag_d20_oth) |> 
  mutate(plot_id = paste(y4_hhid, gardenid, plotid, sep = "-"))
look_for(garden)
garden <- semi_join(garden, maizeplots, by="plot_id")
frq(garden$ag_d14) # many plots are rented out or fallow
# We only keep those plots that are cultivated, even if these plots have been listed as maize fields in HP data.   
garden <- garden |> filter(ag_d14==1)
garden <- garden |> 
  group_by(y4_hhid)|>
  mutate(count_plot=n())|>
  ungroup()
val_labels(garden$ag_d20a)
table(garden$ag_d20a) # in rare cases other corps are listed first, 
# but the kept obs are those with maize as the highest value crop
table(garden$ag_d20_oth)  # 113 cowpeas
garden <- garden |> 
  mutate(intercrop=as.integer(
    if_any(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e, ag_d20_oth), ~ . %in% c(11, 12, 27, 34, 35, 36, 46, "COW PEAS"))
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
modd <- read_dta("data/mlw/ag_mod_d_19.dta") |> 
  select(y4_hhid, gardenid, plotid, ag_d69, ag_d69_oth, ag_d70, ag_d70_oth) |> 
  mutate(plot_id = paste(y4_hhid, gardenid, plotid, sep = "-")) |> 
  select( -y4_hhid,-gardenid, -plotid)
modd <- semi_join(modd, maizeplots, by="plot_id")
table(modd$ag_d70_oth) # 1 beans
modd <- modd |> 
  mutate(rotation=as.integer(
    if_any(c(ag_d69, ag_d69_oth, ag_d70, ag_d70_oth), ~ . %in% c(11, 12, 27, 34, 35, 36, 46, "LABLAB BEANS"))
  ))
look_for(garden)
plot4 <- left_join(garden, modd, by="plot_id") |> 
  select(-ag_d20a, -ag_d20b, -ag_d20c, -ag_d20d, -ag_d20e, -ag_d20_oth, -ag_d69, -ag_d69_oth, -ag_d70, -ag_d70_oth)
remove(garden,modd)
var_label(plot4$intercrop) <- "legume intercropped maize plot"
var_label(plot4$rotation) <- "legume is cultivated before or after"
var_label(plot4$count_plot) <- "number of main maize plots"

### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###
modd <- read_dta("data/mlw/ag_mod_d_19.dta") |> 
  select(y4_hhid, gardenid, plotid, ag_d01) |>
  mutate(plot_id = paste(y4_hhid, gardenid, plotid, sep = "-")) |> 
  rename(id_code=ag_d01)
modd <- semi_join(modd, maizeplots, by="plot_id")
individual <- read_dta("data/mlw/hh_mod_b_19.dta") |> 
  select(y4_hhid, id_code, hh_b03, hh_b05a) |> 
  mutate(female=ifelse(hh_b03==2,1,0))
modd <- left_join(modd, individual, by=c("y4_hhid", "id_code"))
remove(individual)
var_label(modd$female) <- "female primary dm concern. activities on maize plots" # also on crops to be planted
modd <- modd |> select(plot_id, female,hh_b05a )
plot4 <- left_join(plot4, modd, by="plot_id")
plot4 <- plot4 |> rename(age_manager_row=hh_b05a)
remove(modd)

### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
# total consumption is absent if MLW
hdds4 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Malawi", wave==4) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, hh_asset_index, HDDS, share_kg_sold)
hdds4 <- hdds4|> 
  semi_join(maizehh, by="hh_id_merge")
look_for(hdds4) # 246 missing for share_kg_sold

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###
plot4 <- left_join(plot4, hpdata, by="plot_id")
remove(hpdata)
look_for(plot4)
plot4 <- plot4 |> rename(hhid=y4_hhid)
plot4 <- plot4 |> relocate(ea_id_merge,.after = hhid)
remove(maizeplots)
table(plot4$female_manager, plot4$female) # same information.
plot4 <- plot4 |> select(-female)
# manager age is missing for 15 plots.

## Collapse / aggregate ----
mlw4 <- plot4 |> 
  select(-ag_d14, -hh_id_merge,-plot_id_merge, -plot_id_obs, -parcel_id_merge, -parcel_id_obs,
         -respondent_id_obs, -respondent_id_merge,-age_manager_row)
look_for(mlw4)
mlw4basics <- mlw4 |> 
  select(hhid, ea_id_merge, hh_id_obs) |> 
  distinct()
wave4mean <- collap(mlw4,
                    farm_size + age_manager + plot_dist_household + ag_asset_index + count_plot + 
                      soil_fertility_index  ~ hhid,
                    FUN = fmean,
                    na.rm = TRUE)
wave4sum <- collap(mlw4,
                   plot_area_GPS +nb_seasonal_crop +yield_value_LCU + yield_value_USD  ~ hhid,
                   FUN = fsum,
                   na.rm = TRUE)
aggregated_vars <- c("farm_size", "age_manager", "plot_dist_household", "soil_fertility_index",
                     "ag_asset_index", "count_plot", "plot_area_GPS", "nb_seasonal_crop", 
                     "yield_value_LCU", "yield_value_USD")
mlw4 <- mlw4 %>% select(-all_of(aggregated_vars))
look_for(mlw4)
mlw4 <- mlw4 |> 
  select(1, 6:7, 12:23)
look_for(mlw4)
# For all remaining binary variables, I will select the max. Meaning, for example,
# at least one plot is managed by female, at least one maize plot manager has primary education, 
# at least one plot is being intercropped and etc. 
mlw4 <- mlw4 |> 
  mutate(across(c(female_manager, formal_education_manager, primary_education_manager,
                  inorganic_fertilizer, plot_certificate, irrigated),
                as.numeric))
wave4max <- collap(mlw4,
                   ~ hhid, 
                   FUN = fmax,
                   na.rm = TRUE)
mlw4 <- left_join(mlw4basics, wave4mean, by=join_by("hhid"))
mlw4 <- left_join(mlw4, wave4sum, by=join_by("hhid"))
mlw4 <- left_join(mlw4, wave4max, by=join_by("hhid"))
mlw4 <- left_join(mlw4, hdds4, by=c("hhid"="hh_id_merge"))
remove(mlw4basics, wave4max, wave4sum, wave4mean,aggregated_vars, hdds4, plot4)
mlw4 <- mlw4 |> mutate(wave="wave4")

































