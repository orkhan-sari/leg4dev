# Run all scripts and clean further
# Malawi ----
source("scripts/mlw1.R") # 1008 obs
source("scripts/mlw2.R") # 1242 obs
source("scripts/mlw3.R") # 1707 obs
source("scripts/mlw4.R") # 2085 obs
setdiff(names(mlw1), names(mlw2))
setdiff(names(mlw2), names(mlw1))
setdiff(names(mlw1), names(mlw3))
setdiff(names(mlw3), names(mlw1))
setdiff(names(mlw1), names(mlw4))
setdiff(names(mlw4), names(mlw1))
setdiff(names(mlw4), names(mlw3))
setdiff(names(mlw3), names(mlw4))
setdiff(names(mlw3), names(mlw2))
setdiff(names(mlw2), names(mlw3))
# "parcel_owned" "tenure_security"; land tenure data is only collected in 3 and adapted to 1 and 2. Thus not present in 4
dfmlw <-bind_rows(mlw1, mlw2)
dfmlw <-bind_rows(dfmlw, mlw3)
dfmlw <-bind_rows(dfmlw, mlw4)
var_label(dfmlw) <- var_label(mlw3)
# remove(mlw1, mlw2, mlw3, mlw4)
look_for(dfmlw)
var_label(dfmlw$fcs) <- "Food Consumption Score"
dfmlw <- dfmlw |> 
  mutate(region=case_when(
    region==100 ~ 1,
    region==200 ~ 2,
    region==300 ~ 3,
    .default = region),
    year= case_when(wave=="wave1" ~ 2010,
                    wave=="wave2" ~ 2013,
                    wave=="wave3" ~ 2016,
                    wave=="wave4" ~ 2019,
                    .default = 0)
  )
dfmlw$region <- drop_unused_value_labels(dfmlw$region)
# market distance is available fro all hhs in wave1 and some for wave4. Since, the it is measured in distance,
# we will replace the missing in other waves with the average observed from wave 1 and 4.
dfmlw <- dfmlw %>%
  group_by(hh_id_obs) %>%
  mutate(
    market_distance = mean(market_distance, na.rm = TRUE)
  ) %>%
  ungroup()
dfmlw <- dfmlw |> 
  relocate(wave, .after= hh_id_obs) |> 
  relocate(region, .after=wave)
table(dfmlw$rain_shock, dfmlw$wave, useNA = "ifany")
table(dfmlw$flood_shock, dfmlw$wave, useNA = "ifany")
dfmlw <- dfmlw |> 
  mutate(rotation=
           factor(rotation, levels = c(0,1), labels = c("norotation", "rotation")),
         intercrop=
           factor(intercrop, levels = c(0,1), labels = c("purestand", "mixed")),
         tot_other_income = if_else(tot_other_income<0, 0, tot_other_income),
         ln_other_income = log(tot_other_income+1),
         b_enterprise_income=if_else(enterprise_income>0,1,0)
  )
table(dfmlw$rotation, dfmlw$wave)
table(dfmlw$iv_maize, dfmlw$wave)
table(dfmlw$intercrop, dfmlw$wave)
dfmlw <- dfmlw |> 
  mutate(maize_share=plot_area/farm_size)
dfmlw <- dfmlw |> 
  mutate(eco_soil_mng = if_else(
    if_any(c(zero_till, min_till), ~ .x == 1), # crop residue from mlw2 most likely incorrect; not considered
    1L, 0L)
  )
dfmlw <- dfmlw |>
  mutate(weather_shock= if_else(flood_shock | rain_shock | drought_shock ==1, 1, 0, missing = 0)) |> # 0 assumed for missing! 
  relocate(weather_shock, .after = flood_shock)
table(dfmlw$weather_shock, dfmlw$wave, useNA = "ifany")
dfmlw$improved <- NULL
dfmlw <- dfmlw |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs)
table(dfmlw$dupl) # 654 panel hhs for all rounds

## mlw all rounds ====  
## Most likely won't be needed for estimations
dfmlwfull <- dfmlw |> filter(dupl==4) |> select(-dupl) # HH that grown maize as a main crop in all 4 rounds
look_for(dfmlwfull)
table(dfmlwfull$subsistence_ag, dfmlwfull$wave, useNA = "ifany") # 5 obs in wave 3 and 4
dfmlwfull |> 
  group_by(hh_id_obs) |> 
  select(hh_id_obs, wave, age_manager, female_manager) |> 
  filter(any(is.na(age_manager))) |> 
  arrange(hh_id_obs) |> 
  print(n=75) # some missing can be manually replaced. obvious trend in age of manager is the same person
dfmlwfull <- dfmlwfull |> 
  mutate(age_manager=case_when(hh_id_obs=="2002856" & wave=="wave3" ~ 30,
                               hh_id_obs=="2002918" & wave=="wave3" ~ 67,
                               hh_id_obs=="2003228" & wave=="wave3" ~ 61,
                               hh_id_obs=="2004988" & wave=="wave3" ~ 56,
                               .default = age_manager
  )
  )
table(dfmlw$wave, dfmlw$female_edu) # interestingly, reported female education varies between years - increases.
dfmlwfull |> 
  group_by(hh_id_obs) |> 
  filter(any(is.na(primary_education_manager))) |> 
  select(hh_id_obs, primary_education_manager, wave, age_manager, female_manager) |> 
  ungroup() |> 
  arrange(hh_id_obs) |> 
  print(n=64) # to replace missing for cases where we can be sure about the manager
dfmlwfull <- dfmlwfull |> 
  mutate(primary_education_manager=case_when(hh_id_obs=="2002935" & wave=="wave4" ~ 0,
                                             hh_id_obs=="2002949" & wave=="wave4" ~ 0,
                                             .default = primary_education_manager)
  )
# 2004367 manager is 4 years old in wave1 
dfmlwfull <- dfmlwfull |> 
  mutate(legume_intens=
           case_when(rotation=="rotation" | intercrop=="mixed" ~ 1L,
                     .default = 0L)
  )
dfmlwfull <- dfmlwfull |> 
  mutate(wavem=case_when(wave=="wave1" ~ 1,
                         wave=="wave2" ~ 2,
                         wave=="wave3" ~ 3,
                         wave=="wave4" ~ 4,
                         .default = NA_real_))
spatial <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country == "Malawi") |> 
  select(hh_id_obs, wave, lat_modified, lon_modified) |> 
  rename(wavem=wave)
dfmlwfull <- left_join(dfmlwfull, spatial, by=c("hh_id_obs", "wavem")) # 2 missing geo coordinates
dfmlwfull <- left_join(dfmlwfull, rainfall, by=c("lat_modified"="lat", "lon_modified"="lon", "wavem"="wave"))
var_label(dfmlwfull$seasonal_rainfall) <- "Total rainfall during the production season"
var_label(dfmlwfull$average_rainfall) <- "Average rainfall observed since 1990"
var_label(dfmlwfull$sd_rainfall) <- "Standard deviation of mean seasonal rainfall since 1990"
var_label(dfmlwfull$rainfall_diff) <- "Deviations from expected seasonal rainfall"
var_label(dfmlwfull$lag_rainfall) <- "Total rainfall of the lagged year"
look_for(dfmlwfull)
dfmlwfull |> 
  group_by(hh_id_obs) |> 
  select(hh_id_obs, wave, age_manager, female_manager) |> 
  filter(any(is.na(age_manager))) |> 
  arrange(hh_id_obs) |> 
  print(n=75) # some differences are observed over years. 
dfmlwfull <- dfmlwfull |> 
  mutate(age_manager=case_when(hh_id_obs=="2003821" & wave=="wave3" ~ 36,
                               .default = age_manager)
  )
dfmlwfull <- dfmlwfull |> 
  mutate(flat_slope=case_when(slope=="Flat" ~ 1,
                              is.na(slope) ~ NA_real_,
                              .default = 0),
         mod_slope=case_when(slope=="Slight slope" ~ 1,
                             is.na(slope) ~ NA_real_,
                             .default = 0))
frq(dfmlwfull$flat_slope)
frq(dfmlwfull$mod_slope)
dfmlwfull <- dfmlwfull |> 
  group_by(hh_id_obs) |> 
  mutate(
    dis_plot1 = max(plot_dist_household, na.rm = TRUE),       # max within hh_id_obs
    dis_plot = if_else(is.na(plot_dist_household), dis_plot1, plot_dist_household),
    ln_dis_plot = log1p(dis_plot)
  ) %>%
  ungroup()
dfmlwfull$dis_plot1 <- NULL
table(dfmlwfull$region, dfmlwfull$wave)
dfmlwfull <- dfmlwfull |> 
  group_by(hh_id_obs) %>%
  mutate(moved = n_distinct(region, na.rm = TRUE) > 1) |> 
  ungroup()
frq(dfmlwfull$moved) # 2 observations
movedhh <- dfmlwfull |> 
  select(hh_id_obs, hh_a11, region, moved, wave) |> 
  filter(moved=="TRUE")
# for two hhs 2005161 and 2004821, the location was noted as to be the same dwelling but region code is different.
# this is probably an error. 

get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

dfmlwfull <- dfmlwfull |> 
  rename(regions=region) |> 
  group_by(hh_id_obs) %>%
  mutate(region = get_mode(regions)) %>%
  ungroup()
table(dfmlwfull$region, dfmlwfull$wave)


### Adopting neighbors ----
## Adopters
library(geosphere)
# wave 1
dfspatialmlw1 <- dfmlwfull |> 
  filter(wavem==1) |> 
  select(lat_modified, lon_modified, hh_id_obs, legume_intens, wavem) 
dist_matrix <- distm(dfspatialmlw1[, c("lon_modified", "lat_modified")], fun = distHaversine)

# Count number of neighbors with adopted == 1 within 10 km (10,000 m)
within10km_adopted <- sapply(1:nrow(dfspatialmlw1), function(i) {
  neighbors <- which(dist_matrix[i, ] <= 10000 & dfspatialmlw1$hh_id_obs != dfspatialmlw1$hh_id_obs[i])
  sum(dfspatialmlw1$legume_intens[neighbors])
})
dfspatialmlw1$adopted_neighbors_10km <- within10km_adopted
frq(dfspatialmlw1$adopted_neighbors_10km)
dfspatialmlw1 <- dfspatialmlw1 |> 
  select(hh_id_obs, adopted_neighbors_10km, wavem )

# wave 2
dfspatialmlw2 <- dfmlwfull |> 
  filter(wavem==2) |> 
  select(lat_modified, lon_modified, hh_id_obs, legume_intens, wavem) 
dist_matrix <- distm(dfspatialmlw2[, c("lon_modified", "lat_modified")], fun = distHaversine)

# Count number of neighbors with adopted == 1 within 10 km (10,000 m)
within10km_adopted <- sapply(1:nrow(dfspatialmlw2), function(i) {
  neighbors <- which(dist_matrix[i, ] <= 10000 & dfspatialmlw2$hh_id_obs != dfspatialmlw2$hh_id_obs[i])
  sum(dfspatialmlw2$legume_intens[neighbors])
})
dfspatialmlw2$adopted_neighbors_10km <- within10km_adopted
frq(dfspatialmlw2$adopted_neighbors_10km)
dfspatialmlw2 <- dfspatialmlw2 |> 
  select(hh_id_obs, adopted_neighbors_10km, wavem )
# wave3 
dfspatialmlw3 <- dfmlwfull |> 
  filter(wavem==3) |> 
  select(lat_modified, lon_modified, hh_id_obs, legume_intens, wavem) 
dist_matrix <- distm(dfspatialmlw3[, c("lon_modified", "lat_modified")], fun = distHaversine)

# Count number of neighbors with adopted == 1 within 10 km (10,000 m)
within10km_adopted <- sapply(1:nrow(dfspatialmlw3), function(i) {
  neighbors <- which(dist_matrix[i, ] <= 10000 & dfspatialmlw3$hh_id_obs != dfspatialmlw3$hh_id_obs[i])
  sum(dfspatialmlw3$legume_intens[neighbors])
})
dfspatialmlw3$adopted_neighbors_10km <- within10km_adopted
frq(dfspatialmlw3$adopted_neighbors_10km)
dfspatialmlw3 <- dfspatialmlw3 |> 
  select(hh_id_obs, adopted_neighbors_10km, wavem )

# wave4
dfspatialmlw4 <- dfmlwfull |> 
  filter(wavem==4) |> 
  select(lat_modified, lon_modified, hh_id_obs, legume_intens, wavem) 
dist_matrix <- distm(dfspatialmlw4[, c("lon_modified", "lat_modified")], fun = distHaversine)

# Count number of neighbors with adopted == 1 within 10 km (10,000 m)
within10km_adopted <- sapply(1:nrow(dfspatialmlw4), function(i) {
  neighbors <- which(dist_matrix[i, ] <= 10000 & dfspatialmlw4$hh_id_obs != dfspatialmlw4$hh_id_obs[i])
  sum(dfspatialmlw4$legume_intens[neighbors])
})
dfspatialmlw4$adopted_neighbors_10km <- within10km_adopted
frq(dfspatialmlw4$adopted_neighbors_10km)
dfspatialmlw4 <- dfspatialmlw4 |> 
  select(hh_id_obs, adopted_neighbors_10km, wavem )

dfn <- rbind(dfspatialmlw1, dfspatialmlw2)
dfnmlw <- rbind(dfn, dfspatialmlw3)
dfnmlwfull <- rbind(dfnmlw, dfspatialmlw4)
dfmlwfull <- left_join(dfmlwfull, dfnmlwfull, by=c("hh_id_obs", "wavem"))

rm(dfspatialmlw1, dfspatialmlw2, dfspatialmlw3, dfspatialmlw4, dfn, dfnmlw, dfnmlwfull, within10km_adopted, dist_matrix)

write_dta(dfmlwfull, "data/est/dfmlwfull.dta", version = 15)
saveRDS(dfmlwfull, file = "data/est/dfmlwfull.rds")

## ### ### ### ### ###
## mlw three rounds ----
## ### ### ### ### ###
dfmlw3 <- dfmlw |> 
  filter(wave != "wave1")
dfmlw3 <- dfmlw3 |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs)
table(dfmlw3$dupl)
dfmlw3 <- dfmlw3 |> filter(dupl==3) |> select(-dupl) # HH that grown maize as a main crop in past 3 rounds
# source("scripts/rainfall.R")
dfmlw3 <- dfmlw3 |> 
  mutate(legume_intens=
           case_when(rotation=="rotation" | intercrop=="mixed" ~ 1L,
                     .default = 0L)
  )
look_for(dfmlw3)
dfmlw3 <- dfmlw3 |> 
  mutate(wavem=case_when(wave=="wave2" ~ 2,
                         wave=="wave3" ~ 3,
                         wave=="wave4" ~ 4,
                         .default = NA_real_))
spatial <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country == "Malawi") |> 
  select(hh_id_obs, wave, lat_modified, lon_modified) |> 
  rename(wavem=wave)
dfmlw3 <- left_join(dfmlw3, spatial, by=c("hh_id_obs", "wavem")) # 3 missing geo coordinates
dfmlw3 <- left_join(dfmlw3, rainfall, by=c("lat_modified"="lat", "lon_modified"="lon", "wavem"="wave"))
var_label(dfmlw3$seasonal_rainfall) <- "Total rainfall during the production season"
var_label(dfmlw3$average_rainfall) <- "Average rainfall observed since 1990"
var_label(dfmlw3$sd_rainfall) <- "Standard deviation of mean seasonal rainfall since 1990"
var_label(dfmlw3$rainfall_diff) <- "Deviations from expected seasonal rainfall"
var_label(dfmlw3$lag_rainfall) <- "Total rainfall of the lagged year"
look_for(dfmlw3)
rm(spatial, rain_df, rain_df1, rain_df2, rain_df3, rain_df4)
dfmlw3 |> 
  group_by(hh_id_obs) |> 
  select(hh_id_obs, wave, age_manager, female_manager) |> 
  filter(any(is.na(age_manager))) |> 
  arrange(hh_id_obs) |> 
  print(n=75) # some differences are observed over years. 
dfmlw3 |> select(hh_id_obs, wave, elevation) |> view()
dfmlw3 <- dfmlw3 %>%
  group_by(hh_id_obs) %>% 
  mutate(elevation = if_else(is.na(elevation),
                             mean(elevation, na.rm = TRUE),
                             elevation)) %>%
  ungroup()
dfmlw3 <- dfmlw3 |> 
  mutate(age_manager=case_when(hh_id_obs=="2000927" & wave=="wave3" ~ 27,
                               hh_id_obs=="2002018" & wave=="wave3" ~ 27,
                               hh_id_obs=="2002188" & wave=="wave3" ~ 54,
                               hh_id_obs=="2002784" & wave=="wave3" ~ 30,
                               hh_id_obs=="2002856" & wave=="wave3" ~ 30,
                               hh_id_obs=="2002918" & wave=="wave3" ~ 67,
                               hh_id_obs=="2004988" & wave=="wave3" ~ 56,
                               .default = age_manager)
  )
table(dfmlw3$subsistence_ag, dfmlw3$wave, useNA = "ifany") # 3,6,6 obs
dfmlw3 |> 
  group_by(hh_id_obs) |> 
  select(hh_id_obs, wave, subsistence_ag, yield_value_USD, drought_shock) |> 
  filter(any(is.na(yield_value_USD))) |> 
  arrange(hh_id_obs) |> 
  print(n=81) # for all subsistence_ag missing yield and shock are missing too naturally. 
look_for(dfmlw3)
dfmlw3$wave <- as_factor(dfmlw3$wave)
table(dfmlw3$wave, dfmlw3$slope)
dfmlw3 <- dfmlw3 |> 
  mutate(flat_slope=case_when(slope=="Flat" ~ 1,
                              is.na(slope) ~ NA_real_,
                              .default = 0),
         mod_slope=case_when(slope=="Slight slope" ~ 1,
                             is.na(slope) ~ NA_real_,
                             .default = 0))
frq(dfmlw3$flat_slope)
frq(dfmlw3$mod_slope)
dfmlw3 <- dfmlw3 |> 
  group_by(hh_id_obs) |> 
  mutate(
    dis_plot1 = max(plot_dist_household, na.rm = TRUE),       # max within hh_id_obs
    dis_plot = if_else(is.na(plot_dist_household), dis_plot1, plot_dist_household),
    ln_dis_plot = log1p(dis_plot)
  ) %>%
  ungroup()
dfmlw3$dis_plot1 <- NULL
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

dfmlw3 <- dfmlw3 |> 
  rename(regions=region) |> 
  group_by(hh_id_obs) |> 
  mutate(region = get_mode(regions)) |> 
  ungroup()
table(dfmlw3$region, dfmlw3$wave)

### Adopting neighbors ----
## Adopters
library(geosphere)
dfspatialmlw2 <- dfmlw3 |> 
  filter(wavem==2) |> 
  select(lat_modified, lon_modified, hh_id_obs, legume_intens, wavem) 
dist_matrix <- distm(dfspatialmlw2[, c("lon_modified", "lat_modified")], fun = distHaversine)

# Count number of neighbors with adopted == 1 within 10 km (10,000 m)
within10km_adopted <- sapply(1:nrow(dfspatialmlw2), function(i) {
  neighbors <- which(dist_matrix[i, ] <= 10000 & dfspatialmlw2$hh_id_obs != dfspatialmlw2$hh_id_obs[i])
  sum(dfspatialmlw2$legume_intens[neighbors])
})
dfspatialmlw2$adopted_neighbors_10km <- within10km_adopted
frq(dfspatialmlw2$adopted_neighbors_10km)
dfspatialmlw2 <- dfspatialmlw2 |> 
  select(hh_id_obs, adopted_neighbors_10km, wavem )
# wave3 
dfspatialmlw3 <- dfmlw3 |> 
  filter(wavem==3) |> 
  select(lat_modified, lon_modified, hh_id_obs, legume_intens, wavem) 
dist_matrix <- distm(dfspatialmlw3[, c("lon_modified", "lat_modified")], fun = distHaversine)

# Count number of neighbors with adopted == 1 within 10 km (10,000 m)
within10km_adopted <- sapply(1:nrow(dfspatialmlw3), function(i) {
  neighbors <- which(dist_matrix[i, ] <= 10000 & dfspatialmlw3$hh_id_obs != dfspatialmlw3$hh_id_obs[i])
  sum(dfspatialmlw3$legume_intens[neighbors])
})
dfspatialmlw3$adopted_neighbors_10km <- within10km_adopted
frq(dfspatialmlw3$adopted_neighbors_10km)
dfspatialmlw3 <- dfspatialmlw3 |> 
  select(hh_id_obs, adopted_neighbors_10km, wavem )

# wave4
dfspatialmlw4 <- dfmlw3 |> 
  filter(wavem==4) |> 
  select(lat_modified, lon_modified, hh_id_obs, legume_intens, wavem) 
dist_matrix <- distm(dfspatialmlw4[, c("lon_modified", "lat_modified")], fun = distHaversine)

# Count number of neighbors with adopted == 1 within 10 km (10,000 m)
within10km_adopted <- sapply(1:nrow(dfspatialmlw4), function(i) {
  neighbors <- which(dist_matrix[i, ] <= 10000 & dfspatialmlw4$hh_id_obs != dfspatialmlw4$hh_id_obs[i])
  sum(dfspatialmlw4$legume_intens[neighbors])
})
dfspatialmlw4$adopted_neighbors_10km <- within10km_adopted
frq(dfspatialmlw4$adopted_neighbors_10km)
dfspatialmlw4 <- dfspatialmlw4 |> 
  select(hh_id_obs, adopted_neighbors_10km, wavem )

dfn <- rbind(dfspatialmlw2, dfspatialmlw3)
dfnmlw <- rbind(dfn, dfspatialmlw4)
dfmlw3 <- left_join(dfmlw3, dfnmlw, by=c("hh_id_obs", "wavem"))

rm(dfspatialmlw2, dfspatialmlw3, dfspatialmlw4, dfn, dfnmlw, within10km_adopted, dist_matrix)

# dir.create("data/est") # create the folder for estimation STATA datasets
write_dta(dfmlw3, "data/est/dfmlw3.dta", version = 15) # HH that grown maize as a main crop in all 3 rounds
saveRDS(dfmlw3, file = "data/est/dfmlw3.rds")



