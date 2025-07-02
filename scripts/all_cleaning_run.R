# Run all scripts an clean further
# Ethiopia ----
getwd()
source("scripts/eth4.R")
source("scripts/eth5.R")
names(eth4)
names(eth5)
dfeth <-rbind(eth4, eth5) 
dfeth <- dfeth |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup()
table(dfeth$dupl) # 522 panel hhs 
dfeth <- dfeth |> filter(dupl==2) |> select(-dupl)
look_for(dfeth) 
remove(eth4, eth5)
dfeth$wave <- as.factor(dfeth$wave)
dfeth <- dfeth |> 
  relocate(wave, .after= hh_id_obs)
look_for(dfeth)
dfeth |> 
  group_by(household_id) |> 
  filter(any(is.na(primary_education_manager))) |> 
  select(household_id, primary_education_manager, wave) |> 
  ungroup() |> 
  arrange(household_id) |> 
  print(n=50) # for all missing, we know the edu in another wave. we will assume it is the same holder. 
dfeth <- dfeth |> 
  group_by(household_id) |> 
  mutate(
    imputed_primary_edu = if_else(
      is.na(primary_education_manager),
      TRUE,
      FALSE
    ),
    primary_education_manager = if_else(
      is.na(primary_education_manager),
      unique(na.omit(primary_education_manager))[1],
      primary_education_manager
    )
  ) |> 
  ungroup()


# Malawi ----
source("scripts/mlw1.R")
source("scripts/mlw2.R")
source("scripts/mlw3.R")
source("scripts/mlw4.R")
names(mlw2)
names(mlw3) # land tenure data is only collected in this round. 
names(mlw4)
dfmlw <-bind_rows(mlw2, mlw3) 
dfmlw <-bind_rows(dfmlw, mlw4)
dfmlw <- dfmlw |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs)
table(dfmlw$dupl) # 924 panel hhs.
dfmlw <- dfmlw |> filter(dupl==3) |> select(-dupl)
var_label(dfmlw) <- var_label(mlw3)
dfmlw <- dfmlw |> 
  relocate(wave, .after= hh_id_obs)
look_for(dfmlw)
table(dfmlw$wave, dfmlw$female_edu) # interestingly, reported female education varies between years.
