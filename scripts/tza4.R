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
hh14 <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/hh_filters.dta") |> 
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
# Maize - legume intercropping---- 
### ### ### ### ### ### ### ###
plot <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_3a.dta") |> # plot level 
  select(y4_hhid, plotnum, ag3a_03, ag3a_07_1, ag3a_07_2) |> 
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-"))
look_for(plot)
plot <- semi_join(plot, maizeplots, by="plot_id")
table(plot$ag3a_03) 
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
maize_intercrop_plots <- plot |>
  left_join(intercropdata, by = c("y4_hhid", "plotnum")) |>
  filter(!is.na(intercrop) & intercrop == 1)

table(maize_intercrop_plots$ag3a_07_2)

legume_codes <- c(31, 32, 33, 34, 35, 36, 37, 43, 47)

# Filter maize-legume intercropped plots based on legume codes
maize_legume_intercrop <- maize_intercrop_plots |>
  filter(zaocode %in% legume_codes)

# Number of distinct plots with maize-legume intercropping
num_plots_intercrop <- maize_legume_intercrop %>%
  distinct(plot_id) %>%
  nrow()

# Number of distinct households with maize-legume intercropping
num_households_intercrop <- maize_legume_intercrop %>%
  distinct(y4_hhid) %>%
  nrow()

num_plots_intercrop
num_households_intercrop
##There are **438 distinct plots** where maize-legume intercropping has been practiced, spread across **367 distinct households**.

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

LRseason <- read_dta("data/tza/tza_wave4/ag_sec_4a.dta")  # Long rainy season data
SRseason <- read_dta("data/tza/tza_wave4/ag_sec_4b.dta")  # Short rainy season data

#Identify maize plots in the long rainy season
maize_long_rainy <- LRseason |>
  filter(zaocode == 11) |>  # Only keep plots where maize was cultivated in the long rainy season
  select(y4_hhid, plotnum, zaocode) |>  # Keep household and plot ID
  mutate(plot_id_merge = paste(y4_hhid, plotnum, sep = "-"))


#Identify legume plots in the short rainy season
legume_short_rainy <- SRseason |>
  filter(zaocode %in% legume_codes) |>  # Only keep plots where legumes were cultivated in the short rainy season
  select(y4_hhid, plotnum, zaocode) |>  # Keep household and plot ID
  mutate(plot_id_merge = paste(y4_hhid, plotnum, sep = "-"))

# Merge the maize plots from the long rainy season with hpdata (to ensure they are from households with maize as high-value crop)
maize_long_rainy <- maize_long_rainy |> 
  semi_join(hpdata, by = "plot_id_merge")

#Merge the legume plots from the short rainy season with hpdata 
legume_plots_hp <- semi_join(legume_short_rainy, hpdata, by = "plot_id_merge")


# Merge maize plots from the long rainy season with legume plots from the short rainy season 
maize_legume_rotation <- maize_long_rainy |>
  inner_join(legume_plots_hp, by = "plot_id_merge") 

maize_legume_rotation <- maize_legume_rotation |>
  rename(
    y4_hhid = y4_hhid.x,  
    plotnum = plotnum.x,  
    maizecode = zaocode.x,
    legumecode = zaocode.y 
  ) |>
  select(-y4_hhid.y, -plotnum.y)  

summary(maize_legume_rotation)
# Now, we have all the plots that had maize in the long rainy season and legumes in the short rainy season.

# Count the number of households with maize-legume rotation
num_households_rotation <- maize_legume_rotation |> 
  distinct(y4_hhid) |> 
  nrow()

# Count the number of plots with maize-legume rotation
num_plots_rotation <- maize_legume_rotation |> 
  distinct(plot_id_merge) |> 
  nrow()

num_households_rotation
num_plots_rotation
#There are **37 distinct plots** where maize-legume rotation has been practiced, spread across **33 distinct households**.


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
  mutate(female=ifelse(hh_b02==2,1,0)) 

# Rename 'indidy4' to 'id_code' in the 'individual' dataset
individual <- individual |> 
  rename(id_code = indidy4)

modd <- left_join(modd,individual, by=c("y4_hhid", "id_code"))
summary(modd)
remove(individual)
var_label(modd$female) <- "female primary dm concern. activities on maize plots" # also on crops to be planted

summary(plot)
modd <- modd |> select(plot_id, female,hh_b04 )  
plot4 <- left_join(plot, modd, by="plot_id")
plot4 <- plot4 |> rename(age_manager_row=hh_b04)
remove(modd)

plot4 <- plot4 |>
  rename(
    y4_hhid = y4_hhid.x,  
    plotnum = plotnum.x,  
  ) |>
  select(-y4_hhid.y, -plotnum.y)  

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

