# Run all scripts and clean further
# Ethiopia ----
getwd()
source("scripts/eth4.R")
source("scripts/eth5.R")
setdiff(names(eth4), names(eth5))
setdiff(names(eth5), names(eth4))
dfeth <-rbind(eth4, eth5) 
dfeth <- dfeth |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup()
table(dfeth$dupl) # 522 panel hhs
dfeth <- dfeth |> filter(dupl==2) |> select(-dupl)
look_for(dfeth)
frq(dfeth$female_dm) # 1 obs on gender info is missing
dfeth$female_dm[dfeth$female_dm==99] <- NA_real_
dfeth$wave <- as.factor(dfeth$wave)
dfeth <- dfeth |> 
  relocate(wave, .after= hh_id_obs)
look_for(dfeth)
dfeth |> 
  group_by(hh_id_obs) |> 
  filter(any(is.na(primary_education_manager))) |> 
  select(hh_id_obs, primary_education_manager, wave, age_manager, female_manager) |> 
  ungroup() |> 
  arrange(hh_id_obs) |> 
  print(n=32) # to replace missing for cases where we can be sure about the manager
dfeth <- dfeth |> 
  mutate(primary_education_manager=case_when(hh_id_obs=="1004112" & wave=="wave5" ~ 0,
                                             hh_id_obs=="1004184" & wave=="wave5" ~ 0,
                                             hh_id_obs=="1004231" & wave=="wave4" ~ 0,
                                             hh_id_obs=="1006757" & wave=="wave4" ~ 1,
                                             hh_id_obs=="1009508" & wave=="wave5" ~ 0,
                                             .default = primary_education_manager)
         )
look_for(dfeth)
frq(dfeth$saq01)
dfeth <- dfeth |>
  mutate(region=case_when(
    saq01 %in% c(2, 3) ~ 1, # north
    saq01 %in% c(4, 13, 15) ~ 2, # central
    saq01 %in% c(6, 12) ~ 3, # west
    saq01 == 7 ~ 4, # snnp
    .default = NA_real_))
frq(dfeth$region)  
dfeth$region <- factor(dfeth$region, levels = c(1,2,3,4), labels = c("North", "Central", "West", "SNNP"))
frq(dfeth$plot_dist_household) # 532 missing from wave5
table(dfeth$wave, dfeth$plot_dist_household)
dfeth <- dfeth |> 
  group_by(hh_id_obs) |> 
  mutate(
    dis_plot1 = max(plot_dist_household, na.rm = TRUE),       # max within hh_id_obs
    dis_plot = if_else(is.na(plot_dist_household), dis_plot1, plot_dist_household),
    ln_dis_plot = log1p(dis_plot)
  ) %>%
  ungroup()
dfeth$dis_plot <- NULL
dfeth$dis_plot1 <- NULL

#### Consumption ##
dfeth <- dfeth |> 
  mutate(def_totcons_aeq=if_else(wave=="wave5", 244.6/388.2 * nom_totcons_aeq, nom_totcons_aeq),
         def_foodcons_aeq=if_else(wave=="wave5", 244.6/388.2 * nom_foodcons_aeq, nom_foodcons_aeq)) |> 
  relocate(def_totcons_aeq, .after = nom_totcons_aeq)

# 2020 costs deflated to 2017 values using cpi https://data.worldbank.org/indicator/FP.CPI.TOTL?locations=ET
# Main dependent variable is totcons_USD here but the above is developed to check robustness later in the analyses. 
dfeth |> group_by(hh_id_obs) |> select(hh_id_obs, wave, nom_totcons_aeq, def_totcons_aeq) |>
  arrange(hh_id_obs) |> print(n=12)
dfeth |> group_by(hh_id_obs) |> select(hh_id_obs, wave, nom_foodcons_aeq, def_foodcons_aeq) |>
  arrange(hh_id_obs) |> print(n=12)
dfeth <- dfeth |> 
  mutate(rotation=
           factor(rotation, levels = c(0,1), labels = c("norotation", "rotation")),
         tot_other_income = if_else(tot_other_income<0, 0, tot_other_income),
         ln_other_income = log(tot_other_income+1),
         )
table(dfeth$rotation, dfeth$intercrop) # only 19 hhs had intercropped but not rotated
dfeth <- dfeth |> 
  mutate(legume_intens=
           case_when(rotation=="rotation" | intercrop==1 ~ 1L,
                     .default = 0L)
         )
table(dfeth$rotation, dfeth$wave) # 174 and 201 adopters
table(dfeth$improved_maize, dfeth$wave) # 201 and 269 adopters 
dfeth <- dfeth |>
  mutate(weather_shock= if_else(flood_shock | rain_shock | drought_shock ==1, 1, 0, missing = 0)) |> # 0 assumed for missing! 
  relocate(weather_shock, .after = flood_shock)
table(dfeth$weather_shock, dfeth$wave, useNA = "ifany")
dfeth$improved <- NULL
library(knitr)
dfeth |> 
  count(wave, improved_maize, rotation) |> 
  pivot_wider(names_from = rotation, values_from = n, values_fill = 0) |> 
  kable()
dfeth <- dfeth |> 
  mutate(eco_soil_mng = if_else(
    if_any(c(zero_till, min_till, crop_residue), ~ .x == 1), 
    1L, 0L)
    )
table(dfeth$rotation, dfeth$improved_maize)
remove(eth4, eth5)
dfeth <- dfeth |> 
  mutate(maize_share=plot_area_GPS/farm_size)
var_label(dfeth$maize_share) <- "proportion of area dedicated to maize plots"
dfeth <- dfeth |> 
  mutate(year= if_else(wave=="wave4", 2018, 2021))
# source("scripts/rainfall_eth.R")
dfeth <- dfeth |> 
  mutate(wavem=case_when(wave=="wave4" ~ 4,
                         wave=="wave5" ~ 5,
                         .default = NA_real_))
spatial <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country == "Ethiopia") |> 
  select(hh_id_obs, wave, lat_modified, lon_modified) |> 
  rename(wavem=wave)
dfeth <- left_join(dfeth, spatial, by=c("hh_id_obs", "wavem")) # 8 missing geo coordinates
dfeth <- left_join(dfeth, rainfalleth, by=c("lat_modified"="lat", "lon_modified"="lon", "wavem"="wave"))
var_label(dfeth$seasonal_rainfall) <- "Total annaul rainfall"
var_label(dfeth$average_rainfall) <- "Average rainfall observed since 1990"
var_label(dfeth$sd_rainfall) <- "Standard deviation of mean annual rainfall since 1990"
var_label(dfeth$rainfall_diff) <- "Season's deviation from expected annual rainfall"
look_for(dfeth)
dfeth <- dfeth |> 
  mutate(ln_wage_income = log(wage_income+1),
         b_enterprise_income=if_else(enterprise_income>0,1,0)
         )
table(is.na(dfeth$average_rainfall), dfeth$wave) # 8 missing from wave4; replace with observed from wave5 to keep
dfeth <- dfeth |> 
  group_by(hh_id_obs) |> 
  mutate(average_rainfall= if_else(is.na(average_rainfall),
                                   mean(average_rainfall, na.rm = TRUE), average_rainfall)) |> 
  ungroup()
## Adopting neighbors ----
## Adopters
library(geosphere)
dfspatial4 <- dfeth |> 
  filter(wavem==4) |> 
  select(lat_modified, lon_modified, hh_id_obs, legume_intens, wavem) 
dist_matrix <- distm(dfspatial4[, c("lon_modified", "lat_modified")], fun = distHaversine)

# Count number of neighbors with adopted == 1 within 10 km (10,000 m)
within10km_adopted <- sapply(1:nrow(dfspatial4), function(i) {
  neighbors <- which(dist_matrix[i, ] <= 10000 & dfspatial4$hh_id_obs != dfspatial4$hh_id_obs[i])
  sum(dfspatial4$legume_intens[neighbors])
})
dfspatial4$adopted_neighbors_10km <- within10km_adopted
frq(dfspatial4$adopted_neighbors_10km)
dfspatial4 <- dfspatial4 |> 
  select(hh_id_obs, adopted_neighbors_10km, wavem )
# wave5 
dfspatial5 <- dfeth |> 
  filter(wavem==5) |> 
  select(lat_modified, lon_modified, hh_id_obs, legume_intens, wavem) 
dist_matrix <- distm(dfspatial5[, c("lon_modified", "lat_modified")], fun = distHaversine)

# Count number of neighbors with adopted == 1 within 10 km (10,000 m)
within10km_adopted <- sapply(1:nrow(dfspatial5), function(i) {
  neighbors <- which(dist_matrix[i, ] <= 10000 & dfspatial5$hh_id_obs != dfspatial5$hh_id_obs[i])
  sum(dfspatial5$legume_intens[neighbors])
})
dfspatial5$adopted_neighbors_10km <- within10km_adopted
frq(dfspatial5$adopted_neighbors_10km)
dfspatial5 <- dfspatial5 |> 
  select(hh_id_obs, adopted_neighbors_10km, wavem )
dfn <- rbind(dfspatial4, dfspatial5)
dfeth <- left_join(dfeth, dfn, by=c("hh_id_obs", "wavem"))
rm(dfspatial4, dfspatial5, dfn, within10km_adopted, dist_matrix)
# Run nearest adopters script here
# dir.create("data/est") # create the folder for estimation STATA datasets
write_dta(dfeth, "data/est/dfeth.dta", version = 15)
saveRDS(dfeth, file = "data/est/dfeth.rds")











