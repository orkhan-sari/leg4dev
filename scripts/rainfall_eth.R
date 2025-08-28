# Ethiopia ----
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
  filter(country == "Ethiopia", wave %in% c(4,5)) |> 
  select(hh_id_obs, lat_modified, lon_modified) |>
  semi_join(dfeth, by = "hh_id_obs") |> 
  rename(lon = lon_modified, lat = lat_modified) |>
  select(-hh_id_obs) |>
  relocate(lat, .after = lon) |>
  filter(!is.na(lat) & !is.na(lon))
# some hh locations might have changed over the waves. 
spatial <- spatial |> 
  distinct()
dates <- c("1990-01-02","2019-01-01")
# UNCOMMMENT BELOW TO DOWNLOAD
# some days on 2010 was missing. exact_dates = False is better here. 
# raindfeth <- get_chirps(spatial, dates, server = "CHC", exact_dates = FALSE)
# I forgot 2020 and 21 data will download append below. 
rain_dfeth <- raindfeth
remove(dates)
dates <- c("2019-01-02","2022-01-01")
raindfeth20 <- get_chirps(spatial, dates, server = "CHC", exact_dates = FALSE)
remove(dates, spatial)
rain_dfeth <- rbind(rain_dfeth, raindfeth20)
rain_dfeth <- rain_dfeth |> 
  mutate(
    year = year(date),
    month = month(date)
  ) |> 
  filter(month %in% c(5:9))
rain_dfeth <- rain_dfeth |> 
  group_by(id, year, month) |> 
  mutate(sum_month=sum(chirps)) |>  ungroup()
rain_dfeth <- rain_dfeth |>
  mutate(year=as.numeric(year),
         month=as.numeric(month)
         )

# If ANNUAL RAINFAL: 
rain_dfeth <- rain_dfeth |> 
 group_by(id, lon, lat, year) |> #seasonal rainfall is annual rainfall in ETH
 mutate(seasonal_rainfall=sum(chirps, na.rm = TRUE)) |>  ungroup()
rain_dfeth <- rain_dfeth |> 
 distinct(id, lon, lat, year, seasonal_rainfall)

## wave 4 rainfall ----
## Households were visited from Sept-Nov 2018 that is in Meher
## Rotation is defined as "During the last 3 years, have you planted a legume on this [FIELD]?
rain_df1 <- rain_dfeth |>
  filter(!year %in% c(2019, 2020, 2021, 2022))
rain_df1 <- rain_df1 |>
  group_by(id, lon, lat) |> 
  mutate(
    average_rainfall = mean(seasonal_rainfall, na.rm = TRUE),
    sd_rainfall = sd(seasonal_rainfall, na.rm = TRUE)
  ) |> 
  ungroup()
rain_df1 <- rain_df1 |> 
  mutate(rainfall_diff=seasonal_rainfall- average_rainfall,
         rainfall_variation=sd_rainfall/average_rainfall*100
  )
rain_df1 <- rain_df1 |> 
  filter(year==2018) |> 
  select(-id, -year) |> 
  mutate(wave=4)
rain_df1 <- rain_df1 |> 
  distinct()

## wave 5 rainfall ----
## Households were visited from 2021-09-16 	2022-01-31 that is in Meher
## Rotation is defined as "During the last 3 years, have you planted a legume on this [FIELD]?
rain_df2 <- rain_dfeth |> 
  filter(year != 2022)
rain_df2 <- rain_df2 |>
  group_by(id, lon, lat) |> 
  mutate(
    average_rainfall = mean(seasonal_rainfall, na.rm = TRUE),
    sd_rainfall = sd(seasonal_rainfall, na.rm = TRUE)
  ) |> 
  ungroup()
rain_df2 <- rain_df2 |> 
  mutate(rainfall_diff=seasonal_rainfall- average_rainfall,
         rainfall_variation=sd_rainfall/average_rainfall*100
  )
rain_df2 <- rain_df2 |>
  filter(year==2021) |> 
  select(-id, -year) |> 
  mutate(wave=5)
rain_df2 <- rain_df2 |> 
  distinct()
rainfalleth <- bind_rows(rain_df1, rain_df2)
look_for(rainfalleth)
rm(rain_df1, rain_df2, rain_dfeth)


