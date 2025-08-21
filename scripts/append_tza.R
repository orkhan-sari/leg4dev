# Run all scripts and clean further
# Tanzania ----
source("scripts/tza4.R") # 912 obs
source("scripts/tza5.R") # 1191 obs
setdiff(names(tza4), names(tza5))
setdiff(names(tza5), names(tza4))
dftza <-bind_rows(tza4, tza5)
var_label(dftza) <- var_label(tza4)
look_for(dftza)
dftza <- dftza |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs) |> 
  relocate(wave, .after= dupl)
table(dftza$dupl) # 525 hhs. 
dftza <- dftza |> filter(dupl==2) |> select(-dupl)
frq(dftza$wave)
duplicates <- dftza %>%
  group_by(wave, hh_id_obs) %>%
  filter(n() > 1)
duplicates # 6 ons with same ids repeat within wave
dftza <- dftza |> 
  group_by(hh_id_obs) |> 
  filter(n_distinct(wave)==2)
frq(dftza$rotation) # 25
frq(dftza$intercrop_legume) #338
dftza <- dftza |> 
  mutate(legume_intens=
           case_when(rotation==1 | intercrop_legume==1 ~ 1L,
                     .default = 0L)
  )
frq(dftza$legume_intens) # 353
# run rainfall_tza.R here
dftza <- dftza |> 
  mutate(wavem=case_when(wave=="wave4" ~ 4,
                         wave=="wave5" ~ 5,
                         .default = NA_real_))
spatial <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country == "Tanzania") |> 
  select(hh_id_obs, wave, lat_modified, lon_modified) |> 
  rename(wavem=wave)
dftza <- left_join(dftza, spatial, by=c("hh_id_obs", "wavem")) # many missing for wave5
# adopt locations from wave 4 to wave 5
dftza <- dftza %>%
  group_by(hh_id_obs) %>%
  mutate(
    lon_modified = ifelse(is.na(lon_modified), first(lon_modified[wavem == 4], na.rm = TRUE), lon_modified),
    lat_modified = ifelse(is.na(lat_modified), first(lat_modified[wavem == 4], na.rm = TRUE), lat_modified)
  ) %>%
  ungroup()
look_for(dftza)
dftza <- left_join(dftza, rainfalltza, by=c("lat_modified"="lat", "lon_modified"="lon", "wavem"="wave"))
var_label(dftza$seasonal_rainfall) <- "Total rainfall during the production season"
var_label(dftza$average_rainfall) <- "Average rainfall observed since 1990"
var_label(dftza$sd_rainfall) <- "Standard deviation of mean seasonal rainfall since 1990"
look_for(dftza)

## Adopting neighbors ----
## Adopters
library(geosphere)
dfspatial4 <- dftza |> 
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
dfspatial5 <- dftza |> 
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
dftza <- left_join(dftza, dfn, by=c("hh_id_obs", "wavem"))
rm(dfspatial4, dfspatial5, dfn, within10km_adopted, dist_matrix)
write_dta(dfeth, "data/est/dftza.dta", version = 15)
saveRDS(dfeth, file = "data/est/dftza.rds")
