# Rainfall data for research countries
# https://doi.org/10.21105/joss.02419
# Tanzania ----
# Fieldwork for the NPS 2020/21 began in the last week of December 2020 and was completed in January 2022.
# The questionnarie asks about Masika 2020.
pacman::p_load(tidyverse,
               haven,
               chirps,
               foreign,
               remotes,
               data.table,
               labelled,
               collapse,
               sjmisc)
spatial <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country == "Tanzania", wave %in% c(4,5)) |> 
  select(hh_id_obs, lat_modified, lon_modified) |>
  semi_join(dftza, by = "hh_id_obs") |> 
  rename(lon = lon_modified, lat = lat_modified) |>
  select(-hh_id_obs) |>
  relocate(lat, .after = lon) |>
  filter(!is.na(lat) & !is.na(lon))
# some hh locations might have changed over the waves. 
spatial <- spatial |> 
  distinct()
dates <- c("1990-01-02","2021-01-01") 
# UNCOMMMENT BELOW TO DOWNLOAD
# raindftza <- get_chirps(spatial, dates, server = "CHC")
remove(dates)
remove(spatial)
rain_dftz <- raindftza
rain_dftz <- rain_dftz |> 
  mutate(
    year = year(date),
    month = month(date)
  )
rain_dftz <- rain_dftz |> 
  filter(chirps>=0)
rain_dftz <- rain_dftz |> 
  group_by(id, year, month) |> 
  mutate(sum_month=sum(chirps)) |>  ungroup()
rain_dftz <- rain_dftz |>
  mutate(year=as.numeric(year),
         month=as.numeric(month)
         ) |> 
  filter(month %in% c(3:5))
rain_dftz <- rain_dftz |> 
  distinct(id, lon, lat, month, sum_month, .keep_all = TRUE)
rain_dftz <- rain_dftz |> 
  group_by(id, lon, lat, year) |> 
  summarise(seasonal_rainfall = sum(sum_month, na.rm = TRUE), .groups = "drop")

## wave 4 ---- 
rain_df1 <- rain_dftz |>
  filter(year<2015)
rain_df1 <- rain_df1 |>
  group_by(id, lon, lat) |> 
  mutate(
    average_rainfall = mean(seasonal_rainfall, na.rm = TRUE),
    sd_rainfall = sd(seasonal_rainfall, na.rm = TRUE),
    rainfall_variation=sd_rainfall/average_rainfall*100
  ) |> 
  ungroup()
rain_df1 <- rain_df1 |> 
  filter(year==2014)
rain_df1 <- rain_df1 |> 
  select(-id, -year) |> 
  mutate(wave=4)
rain_df1 <- rain_df1 |> 
  distinct()

## wave 5 ---- 
rain_df2 <- rain_dftz |>
  filter(year<2021)
rain_df2 <- rain_df2 |>
  group_by(id, lon, lat) |> 
  mutate(
    average_rainfall = mean(seasonal_rainfall, na.rm = TRUE),
    sd_rainfall = sd(seasonal_rainfall, na.rm = TRUE),
    rainfall_variation=sd_rainfall/average_rainfall*100
  ) |> 
  ungroup()
rain_df2 <- rain_df2 |> 
  filter(year==2020)
rain_df2 <- rain_df2 |> 
  select(-id, -year) |> 
  mutate(wave=5)
rain_df2 <- rain_df2 |> 
  distinct()
rainfalltza <- bind_rows(rain_df1, rain_df2)
rm(rain_df1, rain_df2, rain_dftz)
