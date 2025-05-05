# Run all scripts
getwd()
source("scripts/eth4.R")
source("scripts/eth5.R")
names(wave4)
names(wave5)
dfeth <-rbind(wave4,wave5) 
dfeth <- dfeth |> 
  group_by(household_id) |> mutate(dupl=n()) |> ungroup()
table(dfeth$dupl) # 725 HHs only present in one wave
dfeth <- dfeth |> filter(dupl==2) |> select(-dupl)
look_for(dfeth)
# 898 observations, 449 maize producing households HHs 
remove(wave4, wave5)
dfeth$wave <- as.factor(dfeth$wave)
look_for(dfeth)
source("scripts/mlw3.R")