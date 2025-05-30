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
intercropdata <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4a.dta") |>  # crops by plot
  select("y4_hhid", "plotnum", "zaocode", "zaoname", "ag4a_04") |> 
  rename(intercrop = ag4a_04) %>%
  filter(!is.na(zaocode))
intercropdata <- intercropdata |> 
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-"))
intercropdata <- semi_join(intercropdata, maizeplots, by="plot_id")
n_distinct(intercropdata$plot_id) # 1204 distinct plots
intercropdata <- intercropdata |> 
  filter(zaocode %in% c(11, 31, 32, 33, 34, 35, 36, 37, 43, 47))
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
intercropdata <- collap(intercropdata, intercrop_legume ~plot_id, FUN = fmax)
frq(intercropdata$intercrop_legume ) # 434 maize plots out of 1204 are intercropped with legumes.


# in HP data, keep only plots from the long panel (extended refresh sample not included) 
maizeplots <- semi_join(maizeplots, intercropdata, by=c("plot_id_merge"="plot_id"))
maizehh <- maizeplots |>  distinct(hh_id_merge) # 911 maize HHs in wave 4. 
hpdata <- semi_join(hpdata, maizeplots, by="plot_id")

### ### ### ### ### ### ### ###
# Maize - legume rotation----
### ### ### ### ### ### ### ###
#Concept of crop rotation used: If a household cultivated maize on a plot in the long rainy season, and a legume was 
#grown on the same plot in the short rainy season, then that plot experienced maize–legume rotation.
# 'plot' contains the maize plots that were actually cultivated in the long rainy season, we’ll now build on that to 
#check which of those plots were rotated to legumes in the short rainy season.

LRseason <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4a.dta")  # Long rainy season data
SRseason <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_4b.dta")  # Short rainy season data
#Identify maize plots in the long rainy season
LRseason <- LRseason |>
  filter(zaocode == 11) |>  # Only keep plots where maize was cultivated in the long rainy season
  select(y4_hhid, plotnum, zaocode) |>  # Keep household and plot ID
  mutate(plot_id_merge = paste(y4_hhid, plotnum, sep = "-"))

#Identify legume plots in the short rainy season
SRseason <- SRseason |>
  filter(zaocode %in% legume_codes) |>  # Only keep plots where legumes were cultivated in the short rainy season
  select(y4_hhid, plotnum, zaocode) |>  # Keep household and plot ID
  mutate(plot_id_merge = paste(y4_hhid, plotnum, sep = "-"))
# Merge the maize plots to ensure they are from households with maize as high-value crop
LRseason <- semi_join(LRseason, hpdata, by = "plot_id_merge")
SRseason <- semi_join(SRseason, hpdata, by = "plot_id_merge")
SRseason <- SRseason |> 
  mutate(rotation=1) |> 
  select(plot_id_merge, rotation)
LRseason |> group_by(plot_id_merge) |> 
  filter(n()>1) |> 
  ungroup() 
SRseason |> group_by(plot_id_merge) |> 
  filter(n()>1) |> 
  ungroup() # several duplicates - intercropped. 
SRseason <- SRseason |> 
  distinct(plot_id_merge, .keep_all = TRUE)
LRseason <- LRseason |> 
  select(plot_id_merge)
# Merge maize plots from the long rainy season with legume plots from the short rainy season 
LRseason <- left_join(LRseason, SRseason, by = "plot_id_merge" )
frq(LRseason$rotation)
#There are **37 distinct plots** where maize-legume rotation has been practiced, spread across **33 distinct households**.
LRseason$rotation[is.na(LRseason$rotation)] <- 0
hpdata <- left_join(hpdata, LRseason, by = "plot_id_merge")
remove(LRseason, SRseason)


### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###

modd <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_3a.dta") |> 
  select(y4_hhid, plotnum, ag3a_08_1) |>
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-")) |> 
  rename(id_code=ag3a_08_1)
modd <- semi_join(modd, maizeplots, by="plot_id")
individual <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_b.dta") |> 
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


### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
hdds4 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Tanzania", wave==4) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, hh_asset_index, HDDS, share_kg_sold)
hdds4 <- hdds4|> 
  semi_join(maizehh, by="hh_id_merge")

############################################################################################################################################
### ### ### ### ### ### ### ###
#HH Plot distance
### ### ### ### ### ### ### ###


plot_hh_dist <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_3a.dta") |>  
  select(y4_hhid, plotnum, ag3a_02_1) |>  
  rename(plot_hh_distance = ag3a_02_1) |>  
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-"))

plot_hh_dist <- semi_join(plot_hh_dist, maizeplots, by = "plot_id")

attr(plot_hh_dist$plot_hh_distance, "label") <- "Distance from plot to home (in KMs), self-reported"

look_for(plot_hh_dist)
#No missing values found for plot_hh_distance

### ### ### ### ### ### ### ###
#Soil Type
### ### ### ### ### ### ### ###

soil_type <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_3a.dta") |>  
  select(y4_hhid, plotnum, ag3a_10) |>  
  rename(soil_type = ag3a_10) |>  
  mutate(plot_id = paste(y4_hhid, plotnum, sep = "-"))

soil_type <- semi_join(soil_type, maizeplots, by = "plot_id")

look_for(soil_type)  #No missing values found for soil_type

soil_type %>%
  count(soil_type, sort = TRUE)


### ### ### ### ### ### ### ###
#Subsistence orientation
### ### ### ### ### ### ### ###

# Identify households with a subsistence orientation (did not sell any maize in 2014)

subsistence_ag <- read_dta("data/tza/tza_wave4_2014_15/TZA_2014_NPS-R4_v03_M_STATA11/ag_sec_5a.dta") |>
  select(hh_id_merge = y4_hhid, sell_produce = ag5a_01) |>
  semi_join(maizehh, by = "hh_id_merge")

# Count households by selling status: 1 = Sold, 2 = Did not sell, NA = Missing
subsistence_ag |> count(sell_produce) # Only 1 HH has a missing value.

# Filter households who did not sell maize (sell_produce == 2)
subsistence_only <- subsistence_ag |> filter(sell_produce == 2)
look_for(hdds4) # 405 missing for share_kg_sold

### ### ### ### ### ### ### ### #### ##### #### ### ###
#Auxiliary Check: How Many LR Legume-Growing Households Planted Maize in the SR
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

#How many legume-growing HHs in LR also grew maize in SR?
legume_then_maize_hhs <- inner_join(LR_legume_hhs, SR_maize_hhs, by = "y4_hhid")

n_LR_legume_hhs <- nrow(LR_legume_hhs)
n_LR_legume_and_SR_maize_hhs <- nrow(legume_then_maize_hhs)

cat("Total households that grew legumes in LR:", n_LR_legume_hhs, "\n")
cat("Of them, households that also grew maize in SR:", n_LR_legume_and_SR_maize_hhs, "\n")

# Out of 981 households that grew legumes in the Long Rainy Season (LR),155 also grew maize in the Short Rainy Season (SR)
#Maybe this suggests that most of these households were likely primarily legume growers, with limited transition to maize in SR.
