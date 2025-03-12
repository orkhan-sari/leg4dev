# Run all scripts
getwd()
source("scripts/eth-wave4.R")
source("scripts/eth-wave5.R")
names(wave4)
names(wave5)
dfeth <-rbind(wave4,wave5) 
dfeth <- dfeth |> 
  group_by(household_id) |> mutate(dupl=n())
table(dfeth$dupl) # 780 HHs only present in one wave; 1186 households in both.
dfeth <- dfeth |> filter(dupl==2) |> select(-dupl)
look_for(dfeth)
