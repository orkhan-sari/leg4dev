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
# I forgot 2020 and 21 data will donwload append below. 
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
  )
rain_dfeth <- rain_dfeth |> 
  group_by(id, year, month) |> 
  mutate(sum_month=sum(chirps)) |>  ungroup()
rain_dfeth <- rain_dfeth |>
  mutate(year=as.numeric(year),
         month=as.numeric(month),
         season = case_when(
           month >= 9 ~ paste0(year, "-", year + 1),  # September to December Meher
           month <= 2  ~ paste0(year - 1, "-", year),  # January to February Meher
           TRUE ~ NA_character_)
  ) # Exclude months outside Nov-Apr
# Main rainy season  Meher which corresponds to data collection is from September to January.
rain_dfeth <- rain_dfeth |> 
  filter(month %in% c(9,10,11, 12, 1, 2)) |> 
  filter(season >= "1990-1991", season <= "2021-2022")
rain_dfeth <- rain_dfeth |> 
  distinct(id, lon, lat, month, sum_month, season, .keep_all = TRUE)
rain_dfeth <- rain_dfeth |> 
  group_by(id, lon, lat, season) |> 
  summarise(seasonal_rainfall = sum(sum_month, na.rm = TRUE), .groups = "drop")

# If ANNUAL RAINFAL: 
# rain_dfeth <- rain_dfeth |> 
#   group_by(id, year) |> #seasonal rainfall is annual rainfall in ETH
#   mutate(seasonal_rainfall=sum(chirps, na.rm = TRUE)) |>  ungroup()
# rain_dfeth <- rain_dfeth |> 
#   distinct(id, lon, lat, year, seasonal_rainfall)

## wave 4 rainfall ----
## Households were visited from Sept-Nov 2018 that is in Meher
## Rotation is defined as "During the last 3 years, have you planted a legume on this [FIELD]?
rain_df1 <- rain_dfeth |>
  filter(!season %in% c("2019-2020", "2020-2021", "2021-2022"))
rain_df1 <- rain_df1 |>
  group_by(id, lon, lat) |> 
  mutate(
    average_rainfall = mean(seasonal_rainfall, na.rm = TRUE),
    sd_rainfall = sd(seasonal_rainfall, na.rm = TRUE)
  ) |> 
  ungroup()
rain_df1 <- rain_df1 |> 
  mutate(rainfall_shock=abs((average_rainfall - seasonal_rainfall)/sd_rainfall),
         rainfall_variation=sd_rainfall/average_rainfall*100
  )
rain_df1 <- rain_df1 %>%
  mutate(
    season_start = as.integer(str_sub(season, 1, 4))
  ) %>%
  arrange(id, season_start) %>%
  group_by(id, lon, lat) %>%
  mutate(
    lag_rainfall = lag(seasonal_rainfall, n = 1)
  ) %>%
  ungroup()
rain_df1 <- rain_df1 |> 
  mutate(lag_rainfall_shock=abs((average_rainfall - lag_rainfall)/sd_rainfall))
rain_df1 <- rain_df1 |> 
  filter(season=="2018-2019")
rain_df1 <- rain_df1 |> 
  select(-id, - season_start, -season) |> 
  mutate(wave=4)
rain_df1 <- rain_df1 |> 
  distinct()

## wave 5 rainfall ----
## Households were visited from 2021-09-16 	2022-01-31 that is in Meher
## Rotation is defined as "During the last 3 years, have you planted a legume on this [FIELD]?
rain_df2 <- rain_dfeth
rain_df2 <- rain_df2 |>
  group_by(id, lon, lat) |> 
  mutate(
    average_rainfall = mean(seasonal_rainfall, na.rm = TRUE),
    sd_rainfall = sd(seasonal_rainfall, na.rm = TRUE)
  ) |> 
  ungroup()
rain_df2 <- rain_df2 |> 
  mutate(rainfall_shock=abs((average_rainfall - seasonal_rainfall)/sd_rainfall),
         rainfall_variation=sd_rainfall/average_rainfall*100
  )
rain_df2 <- rain_df2 %>%
  mutate(
    season_start = as.integer(str_sub(season, 1, 4))
  ) %>%
  arrange(id, season_start) %>%
  group_by(id, lon, lat) %>%
  mutate(
    lag_rainfall = lag(seasonal_rainfall, n = 1)
  ) %>%
  ungroup()
rain_df2 <- rain_df2 |> 
  mutate(lag_rainfall_shock=abs((average_rainfall - lag_rainfall)/sd_rainfall))
rain_df2 <- rain_df2 |> 
  filter(season=="2021-2022")
rain_df2 <- rain_df2 |> 
  select(-id, - season_start, -season) |> 
  mutate(wave=5)
rain_df2 <- rain_df2 |> 
  distinct()
rainfalleth <- bind_rows(rain_df1, rain_df2)
look_for(rainfalleth)



