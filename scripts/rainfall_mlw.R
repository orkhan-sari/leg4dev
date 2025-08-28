# Rainfall data for research countries
# https://doi.org/10.21105/joss.02419
# Malawi ----
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
  filter(country == "Malawi", wave !=1) |> 
  select(hh_id_obs, lat_modified, lon_modified) |>
  semi_join(dfmlw3, by = "hh_id_obs") |> 
  rename(lon = lon_modified, lat = lat_modified) |>
  select(-hh_id_obs) |>
  relocate(lat, .after = lon) |>
  filter(!is.na(lat) & !is.na(lon))
# some hh locations might have changed over the waves. 
spatial <- spatial |> 
  distinct()
dates <- c("1990-01-02","2019-01-01") 
# UNCOMMMENT BELOW TO DOWNLOAD
# raindf <- get_chirps(spatial, dates, server = "CHC")
remove(dates)
remove(spatial)
rain_df <- raindf
rain_df <- rain_df |> 
  mutate(
    year = year(date),
    month = month(date)
    )
rain_df <- rain_df |> 
  filter(chirps>=0)
rain_df <- rain_df |> 
  group_by(id, year, month) |> 
  mutate(sum_month=sum(chirps)) |>  ungroup()
rain_df <- rain_df |>
  mutate(year=as.numeric(year),
         month=as.numeric(month),
         season = case_when(
           month >= 11 ~ paste0(year, "-", year + 1),  # November, December
           month <= 4  ~ paste0(year - 1, "-", year),  # January to April
           TRUE ~ NA_character_)
         ) # Exclude months outside Nov-Apr
# Main rainy season is from November to April. Thus we filter accordingly
rain_df <- rain_df |> 
  filter(month %in% c(11, 12, 1, 2, 3, 4)) |> 
  filter(season >= "1990-1991", season <= "2018-2019")
rain_df <- rain_df |> 
  distinct(id, lon, lat, month, sum_month, season, .keep_all = TRUE)
rain_df <- rain_df |> 
  group_by(id, lon, lat, season) |> 
  summarise(seasonal_rainfall = sum(sum_month, na.rm = TRUE), .groups = "drop")

## wave 1 rainfall ----
rain_df1 <- rain_df |>
  filter(!season %in% c("2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", 
                        "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019"))
rain_df1 <- rain_df1 |>
  group_by(id, lon, lat) |> 
  mutate(
    average_rainfall = mean(seasonal_rainfall, na.rm = TRUE),
    sd_rainfall = sd(seasonal_rainfall, na.rm = TRUE)
  )
rain_df1 <- rain_df1 |> 
  mutate(rainfall_diff=seasonal_rainfall- average_rainfall,
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
  filter(season=="2008-2009")
rain_df1 <- rain_df1 |> 
  select(-id, - season_start, -season) |> 
  mutate(wave=1)
rain_df1 <- rain_df1 |> 
  distinct()


## wave 2 rainfall ----
rain_df2 <- rain_df |>
  filter(!season %in% c("2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019"))
rain_df2 <- rain_df2 |>
  group_by(id, lon, lat) |> 
  mutate(
    average_rainfall = mean(seasonal_rainfall, na.rm = TRUE),
    sd_rainfall = sd(seasonal_rainfall, na.rm = TRUE)
  )
rain_df2 <- rain_df2 |> 
  mutate(rainfall_diff=seasonal_rainfall- average_rainfall,
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
  filter(season=="2011-2012")
rain_df2 <- rain_df2 |> 
  select(-id, - season_start, -season) |> 
  mutate(wave=2)
rain_df2 <- rain_df2 |> 
  distinct()


## wave 3 rainfall ----
rain_df3 <- rain_df |>
  filter(!season %in% c("2015-2016", "2016-2017", "2017-2018", "2018-2019"))
rain_df3 <- rain_df3 |>
  group_by(id, lon, lat) |> 
  mutate(
    average_rainfall = mean(seasonal_rainfall, na.rm = TRUE),
    sd_rainfall = sd(seasonal_rainfall, na.rm = TRUE)
  )
rain_df3 <- rain_df3 |> 
  mutate(rainfall_diff=seasonal_rainfall- average_rainfall,
         rainfall_variation=sd_rainfall/average_rainfall*100
         )
rain_df3 <- rain_df3 %>%
  mutate(
    season_start = as.integer(str_sub(season, 1, 4))
  ) %>%
  arrange(id, season_start) %>%
  group_by(id, lon, lat) %>%
  mutate(
    lag_rainfall = lag(seasonal_rainfall, n = 1)
  ) %>%
  ungroup()
rain_df3 <- rain_df3 |> 
  mutate(lag_rainfall_shock=abs((average_rainfall - lag_rainfall)/sd_rainfall))
rain_df3 <- rain_df3 |> 
  filter(season=="2014-2015")
rain_df3 <- rain_df3 |> 
  select(-id, - season_start, -season) |> 
  mutate(wave=3)
rain_df3 <- rain_df3 |> 
  distinct()

## wave 4 rainfall ----
rain_df4 <- rain_df |>
  filter(!season %in% c("2018-2019"))
rain_df4 <- rain_df4 |>
  group_by(id, lon, lat) |> 
  mutate(
    average_rainfall = mean(seasonal_rainfall, na.rm = TRUE),
    sd_rainfall = sd(seasonal_rainfall, na.rm = TRUE)
  )
rain_df4 <- rain_df4 |> 
  mutate(rainfall_diff=seasonal_rainfall- average_rainfall,
         rainfall_variation=sd_rainfall/average_rainfall*100
         )
rain_df4 <- rain_df4 %>%
  mutate(
    season_start = as.integer(str_sub(season, 1, 4))
  ) %>%
  arrange(id, season_start) %>%
  group_by(id, lon, lat) %>%
  mutate(
    lag_rainfall = lag(seasonal_rainfall, n = 1)
  ) %>%
  ungroup()
rain_df4 <- rain_df4 |> 
  mutate(lag_rainfall_shock=abs((average_rainfall - lag_rainfall)/sd_rainfall))
rain_df4 <- rain_df4 |> 
  filter(season=="2017-2018")
rain_df4 <- rain_df4 |> 
  select(-id, - season_start, -season) |> 
  mutate(wave=4)
rain_df4 <- rain_df4 |> 
  distinct()
rainfall <- bind_rows(rain_df1, rain_df2)
rainfall <- bind_rows(rainfall, rain_df3)
rainfall <- bind_rows(rainfall, rain_df4)
look_for(rainfall)

