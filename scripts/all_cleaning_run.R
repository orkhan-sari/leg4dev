# Run all scripts an clean further
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
  mutate(improved_maize=
           factor(improved_maize, levels = c(0,1), labels = c("local", "improved")),
         rotation=
           factor(rotation, levels = c(0,1), labels = c("norotation", "rotation"))
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
  mutate(agr_intens=
           case_when(
             rotation=="rotation" & improved_maize=="local" ~ 2,
             rotation=="norotation" & improved_maize=="improved" ~ 3,
             rotation=="rotation" & improved_maize=="improved" ~ 4,
             .default = 1),
         agr_intens=
           factor(agr_intens, levels = c(1,2,3,4), labels=c("No intensification", "Rotation", "IV", "Rotation and IV"))
         )
table(dfeth$rotation, dfeth$improved_maize)
frq(dfeth$agr_intens)
table(dfeth$wave, dfeth$agr_intens)
remove(eth4, eth5)
dfeth <- dfeth |> 
  mutate(maize_share=plot_area_GPS/farm_size)
var_label(dfeth$maize_share) <- "% area dedicated to maize plots"
# dir.create("data/est") # create the folder for estimation STATA datasets
write_dta(dfeth, "data/est/dfeth.dta", version = 15)
saveRDS(dfeth, file = "data/est/dfeth.rds")

# Malawi ----
source("scripts/mlw1.R") # 1009 obs
source("scripts/mlw2.R") # 1252 obs
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
# hybrid_maize can in mlw1 and 4 be also captured from parcel and crop level questionnaires. There are differences.  
dfmlw <-bind_rows(mlw1, mlw2) # soil quality labels assumed from mlw1
dfmlw <-bind_rows(dfmlw, mlw3)
dfmlw <-bind_rows(dfmlw, mlw4)
var_label(dfmlw) <- var_label(mlw3)
remove(mlw1, mlw2, mlw3, mlw4)
look_for(dfmlw)
table(dfmlw$rain_shock, dfmlw$wave, useNA = "ifany")
table(dfmlw$flood_shock, dfmlw$wave, useNA = "ifany")
dfmlw <- dfmlw |> 
  mutate(hybrid_maize=
           factor(hybrid_maize, levels = c(0,1), labels = c("local", "improved")),
         rotation=
           factor(rotation, levels = c(0,1), labels = c("norotation", "rotation")),
         intercrop=
           factor(intercrop, levels = c(0,1), labels = c("purestand", "mixed"))
         )
table(dfmlw$rotation, dfmlw$wave)
table(dfmlw$hybrid_maize, dfmlw$wave)
table(dfmlw$intercrop, dfmlw$wave)
dfmlw <- dfmlw |>
  mutate(weather_shock= if_else(flood_shock | rain_shock | drought_shock ==1, 1, 0, missing = 0)) |> # 0 assumed for missing! 
  relocate(weather_shock, .after = flood_shock)
table(dfmlw$weather_shock, dfmlw$wave, useNA = "ifany")
dfmlw$improved <- NULL
dfmlw <- dfmlw |> 
  relocate(wave, .after= hh_id_obs)
dfmlw <- dfmlw |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs)
table(dfmlw$dupl) # 654 panel hhs
dfmlw$wave <- as_factor(dfmlw$wave)

## mlw all rounds ====  
dfmlwfull <- dfmlw |> filter(dupl==4) |> select(-dupl) # HH that grown maize as a main crop in all 4 rounds
look_for(dfmlwfull)
table(dfmlwfull$subsistence_ag, dfmlwfull$wave, useNA = "ifany") # 5 obs in wave 3 and 4
dfmlwfull |> 
  group_by(hh_id_obs) |> 
  select(hh_id_obs, wave, subsistence_ag) |> 
  filter(any(is.na(subsistence_ag))) |> 
  arrange(hh_id_obs) |> 
  print(n=50) # some differences are observed over years. 
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
                                             .default = age_manager)
  )
# 2004367 manager is 4 years old in wave1 
dfmlwfull <- dfmlwfull |> 
mutate(agr_intens=
         case_when(
           (rotation=="rotation" | intercrop == "mixed") & hybrid_maize=="local" ~ 1,
           rotation=="norotation" & intercrop == "purestand"  & hybrid_maize=="improved" ~ 2,
           (rotation=="rotation" | intercrop == "mixed") & hybrid_maize=="improved" ~ 3,
           .default = 0),
       agr_intens=
         factor(agr_intens, levels = c(0,1,2,3), labels=c("No intensification", "Diversification", "IV", "Diversification and IV"))
       )
table(dfmlwfull$rotation, dfmlwfull$hybrid_maize)
frq(dfmlwfull$agr_intens)
table(dfmlwfull$wave, dfmlwfull$agr_intens)
write_dta(dfmlwfull, "data/est/dfmlwfull.dta", version = 15)
saveRDS(dfmlwfull, file = "data/est/dfmlwfull.rds")

## mlw three rounds ----
dfmlw3 <- dfmlw |> 
  filter(wave != "wave1")
dfmlw3 <- dfmlw3 |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs)
table(dfmlw3$dupl)
dfmlw3 <- dfmlw3 |> filter(dupl==3) |> select(-dupl) # HH that grown maize as a main crop in past 3 rounds
look_for(dfmlw3)
dfmlw3 |> 
  group_by(hh_id_obs) |> 
  select(hh_id_obs, wave, age_manager, female_manager) |> 
  filter(any(is.na(age_manager))) |> 
  arrange(hh_id_obs) |> 
  print(n=75) # some differences are observed over years. 
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
write_dta(dfmlw3, "data/est/dfmlw3.dta", version = 15) # HH that grown maize as a main crop in all 3 rounds
saveRDS(dfmlw3, file = "data/est/dfmlw3.rds")













