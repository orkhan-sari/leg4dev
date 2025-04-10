# mlw 3 ----
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               labelled,
               collapse)
getwd()
options(scipen = 999)

# identify panel households
hh16 <- read_dta("data/mlw/hh_mod_b_16.dta") |> distinct(y3_hhid) |> rename(hhid=y3_hhid)

### ### ### ### ### ### ### ###
# Maize - legume intercropping----
### ### ### ### ### ### ### ###
### Garden -> Plot
garden <- read_dta("data/mlw/ag_mod_d_16.dta") |> 
  select(y3_hhid, gardenid, plotid, ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e, ag_d20_oth)
val_labels(garden$ag_d20a)
garden <- garden |> 
  filter(if_any(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e), ~ . == 1))
table(garden$ag_d20_oth)  # cowpeas
garden <- garden |> 
  mutate(intercrop=as.integer(
    if_any(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e, ag_d20_oth), ~ . %in% c(11, 12, 27, 34, 35, 36, 46, "COWPEAS"))
  ))
garden <- collap(garden, intercrop ~ y3_hhid, FUN = fmax)
table(garden$intercrop)
# 0   1 
# 998 807

## cross check with crop data----
## be aware that there is a some difference observed in the number of hh intercropping, but this is likely
## due to the others category. I will employ the `garden` data. 
crop <- read_dta("data/mlw/ag_mod_g_16.dta")
# identify maize producing households
maizehh <- crop |> 
  filter(crop_code_collapsed==1) |> 
  distinct(y3_hhid)
look_for(crop)
table(crop$crop_code_collapsed) ## 111 others
crop <- crop |> 
  filter(crop_code_collapsed %in% c(1, 11, 12, 27, 34, 35, 36, 46))
crop |> group_by(crop_code_collapsed) |> count() |> ungroup()
# A tibble: 7 × 2
# 1  1 [MAIZE]               2893
# 2 11 [GROUNDNUT]            487
# 3 27 [GROUND BEAN(NZAMA)]     9
# 4 34 [BEANS]                246
# 5 35 [SOYABEAN]             216
# 6 36 [PIGEONPEA(NANDOLO)]   776
# 7 46 [PEAS]                  66

crop <- crop |> select(y3_hhid, gardenid, plotid, crop_code_collapsed, ag_g01) |> 
  group_by(y3_hhid,gardenid, plotid) |>
  mutate(maize = as.integer(any(crop_code_collapsed == 1))) # maize fields/gardens and plots identified
table(crop$maize) # 4129 plots have some maize
crop <- crop |> filter(maize==1)
crop |> group_by(crop_code_collapsed) |> count(ag_g01) |> ungroup() |> print(n=50) 
# We see many pure stand legumes,  - same plots have legume and maize as pure stand which is not possible. 
# We also see some NA. close inspection shows that this is an error in most cases. for example check hhid: 0448-001
crop <- crop |>
  group_by(y3_hhid,gardenid, plotid) |> 
  mutate(legume=ifelse(crop_code_collapsed %in% c(11, 12, 27, 34, 35, 36, 46),1,0)) |>
  mutate(intercropping=as.integer(n_distinct(legume, na.rm = TRUE) == 2)) |> # there is only maize and legume in data.
  ungroup()
crop <- collap(crop, intercropping ~ y3_hhid+gardenid+plotid, FUN = fmax )   # intercropping in at least one maize plot. 
crop <- collap(crop, intercropping ~ y3_hhid, FUN = fmax) #intercropping at household level. 
maizehh <- left_join(maizehh, crop, by= "y3_hhid") # intercropping in maize producing households. 
table(maizehh$intercropping)
#   0   1 
# 1036  781
remove(crop, maizehh)




























