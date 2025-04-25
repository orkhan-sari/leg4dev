# tza 4 ----
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               labelled,
               collapse)
getwd()
options(scipen = 999)

# identify panel households
hh14 <- read_dta("data/tza/tza_wave4/hh_filters.dta") |> 
  select(y4_hhid) |> 
  distinct(y4_hhid, .keep_all = TRUE) 
 
### ### ### ### ### ### ### ###
# HP data ----
### ### ### ### ### ### ### ###
hpdata <- read_dta("data/hp/Plot_dataset.dta") |>
  filter(country=="Tanzania", wave==4) 
# We concentrate on the plots where maize is the main crop. 
maizeplots <- hpdata |> 
  filter(main_crop=="MAIZE") |> 
  select(hh_id_merge, plot_id_merge) |> 
  mutate(plot_id=plot_id_merge)
maizehh <- maizeplots |>  distinct(hh_id_merge)
hpdata|>
  filter(main_crop=="MAIZE") |> 
  group_by(hh_id_obs, plot_id_obs)|>
  filter(n() > 1)|>
  ungroup() # 0 duplicates - plots are uniquely identified  
hpdata <- hpdata|>
  filter(main_crop=="MAIZE") |> 
  select(hh_id_merge, hh_id_obs, plot_id_merge, plot_id_obs, plot_area_GPS, main_crop,
         farm_size, manager_id_merge, manager_id_obs, age_manager, female_manager, formal_education_manager, 
         primary_education_manager, respondent_id_obs, respondent_id_merge, improved,organic_fertilizer, 
         inorganic_fertilizer, soil_fertility_index, plot_dist_household, ag_asset_index, plot_certificate, 
         drought_shock, rain_shock,flood_shock,irrigated,  nb_seasonal_crop, used_pesticides, yield_value_LCU, 
         yield_value_USD) |>
  mutate(plot_id=plot_id_merge)
look_for(hpdata) 
# Four variables — soil_fertility_index, plot_dist_household, rain_shock, and flood_shock — have substantial missing data, each with 1,631 missing values.



### ### ### ### ### ### ### ###
# Maize - legume intercropping---- ##NOT SURE IF THIS IS RIGHT!
### ### ### ### ### ### ### ###


plot <- read_dta("data/tza/tza_wave4/ag_sec_3a.dta") |> # plot level 
  select(y4_hhid, plotnum, ag3a_03, ag3a_07_1, ag3a_07_2) |> 
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-"))
look_for(plot)
plot <- semi_join(plot, maizeplots, by="plot_id")
table(plot$ag3a_03) # many plots are rented out or fallow
# 1204 Cultivated maize plots. We only keep those plots that are cultivated, even if these plots have been listed as maize fields in HP data.   
plot <- plot |> filter(ag3a_03==1)
plot <- plot |> 
  group_by(y4_hhid)|>
  mutate(count_plot=n())|>
  ungroup()
table(plot$ag3a_07_2)

# but the kept obs are those with maize as the highest value crop

intercropdata <- read_dta("data/tza/tza_wave4/ag_sec_4a.dta") |> 
  select("y4_hhid", "plotnum", "zaocode", "zaoname", "ag4a_04") |> 
  rename(intercrop = ag4a_04) %>%
  filter(!is.na(zaocode))


# Get all maize intercropped plots
maize_intercrop_plots <- plot %>%
  left_join(intercropdata, by = c("y4_hhid", "plotnum")) %>%
  filter(!is.na(intercrop) & intercrop == 1)

table(maize_intercrop_plots$ag3a_07_2)

legume_codes <- c(31, 32, 33, 34, 35, 36, 37, 43, 47)

# Filter maize-legume intercropped plots based on legume codes
maize_legume_intercrop <- maize_intercrop_plots |>
  filter(zaocode %in% legume_codes)


# Also in HP data, keep only plots that have been cultivated. 
plots <- plot |> select(plot_id)
maizeplots <- semi_join(maizeplots, plots, by="plot_id")
maizehh <- maizeplots |>  distinct(hh_id_merge)
hpdata <- semi_join(hpdata, maizeplots, by="plot_id")
remove(plots)


### ### ### ### ### ### ### ###
# Maize - legume rotation----
### ### ### ### ### ### ### ###
#Concept of crop rotation used: If a household cultivated maize on a plot in the long rainy season, and a legume was 
#grown on the same plot in the short rainy season, then that plot experienced maize–legume rotation.
# 'plot' contains the maize plots that were actually cultivated in the long rainy season, we’ll now build on that to 
#check which of those plots were rotated to legumes in the short rainy season.

modd <- read_dta("data/tza/tza_wave4/ag_sec_4b.dta") |> 
  select(y4_hhid, plotnum, zaocode, zaoname, ag4b_04) |> 
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-")) |>
  rename(intercrop = ag4b_04)
  
modd <- semi_join(modd, maizeplots, by="plot_id")

legume_codes <- c(31, 32, 33, 34, 35, 36, 37, 43, 47)

#Filter short season data to legume plots
short_legumes <- modd |> 
  filter(zaocode %in% legume_codes) |> 
  select(plot_id, zaocode, zaoname) |> 
  distinct()

# Add rotation info to plot dataset
plot_rotation <- plot |> 
  left_join(short_legumes |> mutate(maize_legume_rotation = 1), by = "plot_id") |> 
  mutate(maize_legume_rotation = replace_na(maize_legume_rotation, 0))

var_label(plot_rotation$maize_legume_rotation) <- "Legume cultivated in short rainy season on the same plot"

plot_rotation <- plot_rotation |> 
  rename( rotation = maize_legume_rotation)

table(plot_rotation$rotation)
##Only 44 plots! :( 

# Check for duplicates in plot_rotation
plot_rotation |> 
  group_by(plot_id) |> 
  summarise(n = n()) |> 
  filter(n > 1)

# Check for duplicates in modd
modd |> 
  group_by(plot_id) |> 
  summarise(n = n()) |> 
  filter(n > 1)


plot4 <- plot_rotation |> 
  left_join(select(modd, plot_id, intercrop), by = "plot_id", relationship = "many-to-many")  ##Not sure if this is the right code.


remove(plot,modd)
var_label(plot4$intercrop) <- "legume intercropped maize plot"
var_label(plot4$rotation) <- "legume is cultivated after"
var_label(plot4$count_plot) <- "number of main maize plots"


### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###

modd <- read_dta("data/tza/tza_wave4/ag_sec_3a.dta") |> 
  select(y4_hhid, plotnum, ag3a_08_1) |>
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-")) |> 
  rename(id_code=ag3a_08_1)
modd <- semi_join(modd, maizeplots, by="plot_id")
individual <- read_dta("data/tza/tza_wave4/hh_sec_b.dta") |> 
  select(y4_hhid, indidy4, hh_b02, hh_b04) |> 
  mutate(female=ifelse(hh_b02==2,1,0)) |>
  rename(id_code=indidy4)
modd <- left_join(modd,individual, by=c("y4_hhid", "id_code"))
remove(individual)
var_label(modd$female) <- "female primary dm concern. activities on maize plots" # also on crops to be planted
modd <- modd |> select(plot_id, female,hh_b04 )  
plot4 <- left_join(plot4, modd, by="plot_id")
plot4 <- plot4 |> rename(age_manager_row=hh_b04)
remove(modd)


### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
hdds4 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Tanzania", wave==4) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, hh_asset_index, HDDS, share_kg_sold)
hdds4 <- hdds4|> 
  semi_join(maizehh, by="hh_id_merge")
look_for(hdds4) # 405 missing for share_kg_sold
remove(maizehh)


### ### ### ### ### ### ### ###
# Other variables---- 
### ### ### ### ### ### ### ###
community <- read_dta("data/tza/tza_wave4/com_sec_cb.dta") |> 
  select(occ,y4_cluster,cboa,cb0,cb3)

market_dist <- community |> 
  filter(cb0 == 29) |>  
  select(y4_cluster, market_distance = cb3)

community %>%
  count(y4_cluster) %>%
  filter(n > 1) # one copy 
community <- community |> distinct(y4_cluster,.keep_all = TRUE)


### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###
plot4 <- left_join(plot4, hpdata, by="plot_id")
remove(hpdata)
look_for(plot4)
plot4 <- plot4 |> rename(hhid=y4_hhid)
hh14 <- hh14 |> 
  rename(hhid = y4_hhid)
plot4 <- left_join(plot4, hh14, by="hhid")

remove(community, maizeplots)

table(plot4$female_manager, plot4$female) # same information?.
plot4 <- plot4 |> select(-female)

look_for(plot4, "age_manager")
# manager age is missing for 2 plots.


## Collapse / aggregate ----
tza4 <- plot4 |> 
  select(-ag3a_03, -hh_id_merge,-plot_id_merge, -plot_id_obs,
         -respondent_id_obs, -respondent_id_merge,-age_manager_row)
look_for(tza4)
wave4mean <- collap(tza4,
                    farm_size + age_manager + plot_dist_household + ag_asset_index + count_plot + 
                      soil_fertility_index  ~ hhid,
                    FUN = fmean,
                    na.rm = TRUE)
wave4sum <- collap(tza4,
                   plot_area_GPS +nb_seasonal_crop +yield_value_LCU + yield_value_USD  ~ hhid,
                   FUN = fsum,
                   na.rm = TRUE)
aggregated_vars <- c("farm_size", "age_manager", "plot_dist_household", "soil_fertility_index",
                     "ag_asset_index", "count_plot", "plot_area_GPS", "nb_seasonal_crop", 
                     "yield_value_LCU", "yield_value_USD")
tza4 <- tza4 %>% select(-all_of(aggregated_vars))
look_for(tza4)
tza4 <- tza4 |> 
  select(1, 2, 6:8, 11:22)
look_for(tza4)
# For all remaining binary variables, I will select the max. Meaning, for example,
# at least one plot is managed by female, at least one maize plot manager has primary education, 
# at least one plot is being intercropped and etc. 
wave4max <- collap(tza4,
                   ~ hhid, 
                   FUN = fmax,
                   na.rm = TRUE)
tza4 <- left_join(wave4mean, wave4sum, by=join_by("hhid"))
tza4 <- left_join(tza4, wave4max, by=join_by("hhid"))
tza4 <- left_join(tza4, hdds4, by=c("hhid"="hh_id_merge"))
remove(wave4max, wave4sum,wave4mean,aggregated_vars, hdds4, plot4)
tza4 <- tza4 |> mutate(wave="wave4")


