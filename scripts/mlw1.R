# mlw 1 ----
## necessary libraries
pacman::p_load(tidyverse,
               haven,
               tibble,
               labelled,
               collapse,
               sjmisc)
getwd()
options(scipen = 999)

### ### ### ### ### ### ### ###
# HP data ----
### ### ### ### ### ### ### ###
hpdata <- read_dta("data/hp/Plot_dataset.dta") |>
  filter(country=="Malawi", wave==1) 
# We concentrate on the plots where maize is the main crop. 
maizeplots <- hpdata |> 
  filter(main_crop=="MAIZE") |> 
  select(hh_id_merge, parcel_id_merge, plot_id_merge) |> 
  mutate(plot_id=plot_id_merge)
maizehh <- maizeplots |>  distinct(hh_id_merge)
hpdata|>
  filter(main_crop=="MAIZE") |> 
  group_by(hh_id_obs, parcel_id_obs, plot_id_obs)|>
  filter(n() > 1)|>
  ungroup() # 0 duplicates - plots are uniquely identified  
hpdata <- hpdata|>
  filter(main_crop=="MAIZE") |> 
  mutate(yield_value_USD= yield_value_USD * (share_value_LEGUMES + share_value_MAIZE)) |> 
  select(hh_id_merge, hh_id_obs, ea_id_merge, plot_id_merge, plot_id_obs, parcel_id_obs, parcel_id_merge, plot_area_GPS, main_crop,
         farm_size, manager_id_merge, manager_id_obs, age_manager, female_manager, formal_education_manager, 
         primary_education_manager, twi, improved, soil_fertility_index, elevation,
         plot_dist_household, ag_asset_index, plot_certificate, drought_shock, rain_shock, flood_shock,
         irrigated, nb_seasonal_crop, used_pesticides, yield_value_USD) |>
  mutate(plot_id=plot_id_merge)
hpdata <- hpdata |>
  group_by(hh_id_obs) |> 
  mutate(crop_count=sum(nb_seasonal_crop)) |> 
  relocate(crop_count, .after = nb_seasonal_crop)
look_for(hpdata) # many missing, especially improved seeds

### ### ### ### ### ### ### ###
# Crop diversification ----
### ### ### ### ### ### ### ###

## Maize - legume intercropping----
### ### ### ### ### ### ### ###
### Garden means parcel; Garden -> Plot
garden <- read_dta("data/mlw/ag_mod_d_10.dta") |>   # plot level 
  select(case_id, ag_d00, ag_d14, ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e) |> 
  mutate(plot_id = paste(case_id, ag_d00, sep = "-"))
look_for(garden, details = "none")
garden <- semi_join(garden, maizeplots, by="plot_id")
frq(garden$ag_d14) # some are fallow and missing
# We only keep those plots that are cultivated, even if these plots have been listed as maize fields in HP data.   
garden <- garden |> filter(ag_d14==1)
frq(garden$ag_d20a)
garden <- garden |> 
  group_by(case_id)|>
  mutate(count_plot=n())|>
  ungroup()
val_labels(garden$ag_d20a)
table(garden$ag_d20a) # in very rare cases other crops are listed first, 
# but the kept obs are those with maize as the highest value crop
garden <- garden |> 
  mutate(intercrop=as.integer(
    if_any(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e), ~ . %in% c(11:16, 27, 34:36, 46))
  ),
  hybrid_maize=as.integer(
    if_any(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e), ~ . %in% c(2:4))
  ))

## Improved maize  ----
## plot and crop sections differ. will keep both for now. 
seed <- read_dta("data/mlw/ag_mod_g_10.dta") |> 
  mutate(plot_id = paste(case_id, ag_g0b, sep = "-")) |> 
  filter(ag_g0a==1) |> 
  select(case_id, plot_id, ag_g0d)
seed <- semi_join(seed, maizeplots, by="plot_id")
frq(seed$ag_g0d)
seed <- seed |> filter(ag_g0d %in% c(1:4))
#-------------------------------------------------# 
# Unlike mlw3 and 4, here the question on type was not directly asked.
# Thus will generate from the crop code ag_g0b
seed <- seed |> 
  mutate(hybrid_maize1= case_when(ag_g0d %in% c(2:4) ~ 1,
                                  .default = 0)) |> 
  select(plot_id, hybrid_maize1)
frq(seed$hybrid_maize1)
garden <- left_join(garden, seed, by= "plot_id")
frq(garden$hybrid_maize1)
garden$hybrid_maize1[is.na(garden$hybrid_maize1)] <- 0
table(garden$hybrid_maize, garden$hybrid_maize1) # many differences. will keep only when both reported
remove(seed)
garden <- garden |> 
  mutate(iv_maize= if_else(hybrid_maize1==1 & hybrid_maize==1 , 1 , 0)) |> 
  select(-hybrid_maize, -hybrid_maize1)
# Also in HP data, keep only plots that have been cultivated. 
plots <- garden |> select(plot_id)
maizeplots <- semi_join(maizeplots, plots, by="plot_id")
maizehh <- maizeplots |>  distinct(hh_id_merge)
hpdata <- semi_join(hpdata, maizeplots, by="plot_id")
remove(plots)

## Maize - legume rotation----
# If legume was cultivated after the rainy season
modm <- read_dta("data/mlw/ag_mod_m_10.dta") |> 
  select(case_id, ag_m0b, ag_m0d) |> 
  filter(!is.na(ag_m0d))
# plot id is with D's (dry) but with R's in the rainy season
library(stringr)
modm <- modm |> 
  mutate(ag_m0b = str_replace(ag_m0b, "^D", "R")) |> 
  mutate(plot_id = paste(case_id, ag_m0b, sep = "-"))
val_labels(modm$ag_m0d)
modm <- modm |> 
  mutate(rotation=case_when(ag_m0d %in% c(11:16, 27, 34:36, 46) ~ 1,
                            .default = 0)) |> 
  filter(rotation==1) # only 24 observations 
modm |> 
  group_by(plot_id)|>
  filter(n() > 1)|>
  ungroup() # no duplicate
modm <- modm |> select(plot_id, rotation)
garden <- left_join(garden, modm, by="plot_id")
frq(garden$rotation) # 20 plots only.
garden$rotation[is.na(garden$rotation)] <- 0
plot1 <- garden |> 
  select(case_id, plot_id, count_plot, intercrop, rotation, iv_maize)
remove(garden,modm)
var_label(plot1$intercrop) <- "=1 if legume intercropped maize plot"
var_label(plot1$rotation) <- "=1 if legume is cultivated after" # ONLY AFTER CAN BE IDENTIFIED
var_label(plot1$count_plot) <- "number of main maize plots"
var_label(plot1$iv_maize) <- "=1 if improved maize was cultivated plot section"

### ### ### ### ### ### ### ###
# Soil management ----
# NO DATA ON TILLAGE OR RESIDUE RETENTION
soilman <-  read_dta("data/mlw/ag_mod_d_10.dta") |>   # plot level 
  select(case_id, ag_d00, ag_d01, ag_d36, ag_d38) |> 
  mutate(plot_id = paste(case_id, ag_d00, sep = "-"))
soilman <- semi_join(soilman, maizeplots, by="plot_id")
look_for(soilman)
soilman <- soilman |> 
  mutate(inorganic_fertilizer= if_else(!is.na(ag_d38) & ag_d38== 1 , 1, 0),
         organic_fertilizer= if_else(!is.na(ag_d36) & ag_d36== 1 , 1, 0)
  )
frq(soilman$organic_fertilizer)
frq(soilman$inorganic_fertilizer)
soilman <- soilman |> 
  select(plot_id, inorganic_fertilizer, organic_fertilizer)
plot1 <- left_join(plot1, soilman, by="plot_id")
remove(soilman)
var_label(plot1$organic_fertilizer) <- "organic fertilizer applied to maize plot"
var_label(plot1$inorganic_fertilizer) <- "inorganic fertilizer applied to maize plot"

## Soil type ---- 
modd <-  read_dta("data/mlw/ag_mod_d_10.dta") |>   # plot level 
  select(case_id, ag_d00, ag_d01, ag_d21) |> 
  mutate(plot_id = paste(case_id, ag_d00, sep = "-")) |> 
  rename(id_code=ag_d01)
modd <- semi_join(modd, maizeplots, by="plot_id")
look_for(modd)
frq(modd$ag_d21)
modd <- modd |> 
  mutate(ag_d21 = as.numeric(modd$ag_d21),
         soil_type=case_when(ag_d21==3 ~ 2,
                             ag_d21==1 ~ 1,
                             .default = 0))
modd <- modd |> 
  mutate(soil= factor(soil_type, levels = c(0,1,2), labels = c("Other or mix", "Sandy", "Clay"))) |> 
  select(plot_id, soil)
plot1 <- left_join(plot1, modd, by="plot_id")
var_label(plot1$soil) <- "HH dominant soil type"

## Slop ----
modd <-  read_dta("data/mlw/ag_mod_d_10.dta") |> 
  select(case_id, ag_d00, ag_d26) |> 
  mutate(plot_id = paste(case_id, ag_d00, sep = "-"))
frq(modd$ag_d26) # do mode max again, if there is a steep slope that will be captured this way too. 
modd <- modd |>
  mutate(ag_d26 = as.numeric(modd$ag_d26), 
         slope= factor(ag_d26, levels = c(1,2,3,4), labels = c("Flat", "Slight slope", "Moderate slope", "Steep"))) |> 
  select(plot_id, slope)
frq(modd$slope)
plot1 <- left_join(plot1, modd, by="plot_id")
var_label(plot1$slope) <- "Dominant slope"
remove(modd)

### ### ### ### ### ### ### ###
# Gender: measures of women's status ####
### ### ### ### ### ### ### ###
## DM at plot level ----
modd <-  read_dta("data/mlw/ag_mod_d_10.dta") |>   # plot level 
  select(case_id, ag_d00, ag_d01) |> 
  mutate(plot_id = paste(case_id, ag_d00, sep = "-")) |> 
  rename(id_code=ag_d01)
modd <- semi_join(modd, maizeplots, by="plot_id")
individual <- read_dta("data/mlw/hh_mod_b_10.dta") |> 
  select(case_id, id_code, hh_b01, hh_b03, hh_b05a) |> 
  mutate(female=ifelse(hh_b03==2,1,0))
modd <- left_join(modd, individual, by=c("case_id", "id_code"))
var_label(modd$female) <- "female primary dm concern. activities on maize plots" # also on crops to be planted
modd <- modd |> select(plot_id, female, hh_b05a)
plot1 <- left_join(plot1, modd, by="plot_id")
plot1 <- plot1 |> rename(age_manager_row=hh_b05a)
remove(modd)

## DM on crop income ----
sales <- read_dta("data/mlw/ag_mod_i_10.dta") |> 
  select(case_id, ag_i14a, ag_i14b) |> 
  filter(!is.na(ag_i14a))
sales <- semi_join(sales, maizehh, by=c("case_id" = "hh_id_merge"))
sales <- sales |> 
  rename(id_code=ag_i14a)
individual$hh_b05a <- NULL # drop age
individual$hh_b03 <- NULL
individual$hh_b01 <- NULL
sales <- left_join(sales,individual, by=c("case_id", "id_code"))
look_for(sales)
sales$id_code <- NULL
sales <- sales |> 
  rename(id_code=ag_i14b) # now second decision-maker
sales <- left_join(sales,individual, by=c("case_id", "id_code"))
sales <- sales |> 
  mutate(female_dm=case_when(female.x==1 & female.y==1 ~ 3, #female
                             female.x==1 & is.na(female.y) ~ 3, #female
                             is.na(female.x) & female.y==1 ~ 3, #female
                             female.x==0 & female.y==1 ~ 2,
                             female.x==1 & female.y==0 ~ 2, # joint
                             female.x==0 & female.y==0 ~ 1, # male
                             is.na(female.x) & female.y==0 ~ 1, # male
                             female.x==0 & is.na(female.y) ~1, 
                             .default = 99)
  )
frq(sales$female_dm)
look_for(sales)
sales <- sales[,c(1,5)]
dim(sales)
n_distinct(sales$case_id)
sales <- collap(sales, female_dm ~ case_id, FUN = fmax)
sales <- sales |> 
  mutate(female_dm= factor(female_dm, levels = c(1,2,3), labels = c("Female", "Joint", "Male")))
frq(sales$female_dm)
sales <- sales |> rename(hhid=case_id)
rm(individual)
# Unfortunately, there are many missing values for the question on
# Who in the household makes the decision concerning the use of the [CROP]? 
# Also we find many missing on crop income in next waves, because many did not sell and thus did not make any decisions 
rm(maizeplots)



## Asset ownership ---- 
## no data available on owners

## Education ----
## variable of interest: if HH has an educated adult female
sex <- read_dta("data/mlw/hh_mod_b_10.dta") |> 
  select(case_id, id_code, hh_b03, hh_b05a)
individual <- read_dta("data/mlw/hh_mod_c_10.dta") |> 
  select(case_id, id_code, hh_c05a, hh_c05b, hh_c08)
individual <- left_join(individual, sex, by=c("case_id", "id_code"))
individual <- individual |> 
  mutate(female=ifelse(hh_b03==2,1,0)) |> 
  filter(female==1) |> 
  filter(hh_b05a>17)
individual <- semi_join(individual, maizehh, by=c("case_id" = "hh_id_merge"))
frq(individual$hh_c08) # >7
look_for(individual)
individual$hh_c08 <- as.numeric(individual$hh_c08)
individual <- individual |> 
  filter(!is.na(hh_c08)) |> 
  mutate(female_edu=ifelse(hh_c08 > 7,1,0)) |> # =1 if hh has educated female with at least primary education completed. 
  select(case_id, female_edu)
frq(individual$female_edu)
individual <- collap(individual, female_edu ~ case_id, FUN = fmax)
remove(sex)

### ### ### ### ### ### ### ###
# Nutrition indicators ---- 
### ### ### ### ### ### ### ###
# HDDS
# total consumption is absent if MLW
hdds1 <- read_dta("data/hp/Household_dataset.dta") |>
  filter(country=="Malawi", wave==1) |>
  select(hh_id_merge, hh_size, hh_shock, hh_dependency_ratio, hh_asset_index, HDDS, share_kg_sold)
hdds1 <- hdds1|> 
  semi_join(maizehh, by="hh_id_merge")
look_for(hdds1) # 124 missing for share_kg_sold

# FCS
fcs <- read_dta("data/mlw/hh_mod_g2_10.dta")
look_for(fcs)
frq(fcs$hh_g08b)
fcs$hh_g08a <- as.factor(fcs$hh_g08a)
table(fcs$hh_g08b, fcs$hh_g08c)
fcs <- fcs |> 
  group_by(case_id) %>%
  mutate(
    sum_A_B = sum(hh_g08c[hh_g08a   %in% c("A", "B")], na.rm = TRUE),
    sum_A_B = case_when(sum_A_B > 7 ~ 7,
                        .default = sum_A_B),
    count = case_when(hh_g08a== "A" ~ sum_A_B,
                      .default = hh_g08c)
    ) |> 
  ungroup()
fcs <- fcs |> 
  filter(hh_g08a != "B")
table(fcs$hh_g08b,fcs$count)
fcs <- fcs |> 
  mutate(count=if_else(count > 7, 7, count))
table(fcs$hh_g08b,fcs$hh_g08a)
fcs <- fcs |> 
  mutate(total=case_when(hh_g08a == "A" ~ count * 2,
                         hh_g08a == "C" ~ count * 3,
                         hh_g08a == "D" ~ count * 1,
                         hh_g08a == "E" ~ count * 4,
                         hh_g08a == "F" ~ count * 1,
                         hh_g08a == "G" ~ count * 4,
                         hh_g08a == "H" ~ count * 0.5,
                         hh_g08a == "I" ~ count * 0.5,
                         hh_g08a == "J" ~ count * 0)
         )
fcs <- fcs |> 
  group_by(case_id) |> 
  summarise(fcs=sum(total)) |> 
  ungroup()
fcs <- semi_join(fcs, maizehh, by=c("case_id" = "hh_id_merge"))
hdds1 <- left_join(hdds1, fcs, by=c("hh_id_merge" = "case_id"))
rm(fcs)

# Food security
fs <- read_dta("data/mlw/hh_mod_h_10.dta") |> 
  select(case_id, hh_h01, hh_h02a, hh_h02b, hh_h02c, hh_h02d, hh_h02e) |> 
  filter(hh_h01==1) |> 
  select(-hh_h01)
look_for(fs)
fs <- fs |> 
  mutate(hh_h02d  = if_else(is.na(hh_h02d), 0, hh_h02d),
         csi=(hh_h02a + hh_h02b + hh_h02c + hh_h02d *3 + hh_h02e * 2)
  ) |> 
  select(case_id, csi)
fs <- semi_join(fs, maizehh, by=c("case_id" = "hh_id_merge"))
hdds1 <- left_join(hdds1, fs, by=c("hh_id_merge" = "case_id"))
look_for(hdds1)
summary(hdds1$csi)
hdds1$csi[is.na(hdds1$csi)] <- 0
var_label(hdds1$csi) <- "comparative (reduced) coping strategy index "

### ### ### ### ### ### ### ###
# Final data  ----
### ### ### ### ### ### ### ###
plot1 <- left_join(plot1, hpdata, by="plot_id")
remove(hpdata)
look_for(plot1)
plot1 <- plot1 |> rename(hhid=case_id)
plot1 <- plot1 |> relocate(ea_id_merge,.after = hhid)
table(plot1$female_manager, plot1$female) # same information. managers are decision-makers 
plot1 <- plot1 |> select(-female)
# manager age is missing for 13 plots like in hpdata. can be dropped. 

## Collapse / aggregate ----
mlw1 <- plot1 |> 
  select(-hh_id_merge, -plot_id_merge, -plot_id_obs, -parcel_id_merge, -parcel_id_obs, -age_manager_row, -nb_seasonal_crop)
look_for(mlw1)
# improved, flood_shock, and plot_certificate missing for all. drop
mlw1 <- mlw1 |>
  select(-improved, -flood_shock, -plot_certificate)
n_distinct(mlw1$hh_id_obs) # 1009 hh_id_obs 1361 obs
n_distinct(mlw1$hhid)
look_for(mlw1)
mlw1 <- mlw1 |> 
  group_by(hh_id_obs) |> 
  mutate(plot_area=sum(plot_area_GPS, na.rm = TRUE),
         yield_value_USD=sum(yield_value_USD, na.rm = TRUE)) |> 
  ungroup() |> 
  relocate(plot_area, .after = plot_area_GPS)
# control if hh used any organic or inorganic fertilizer in intercrop field or any other field. 
mlw1 <- mlw1 |> 
  group_by(hh_id_obs) |> 
  mutate(iv_maize = max(iv_maize, na.rm = TRUE),
         inorganic_fertilizer = max(inorganic_fertilizer, na.rm = TRUE),
         organic_fertilizer = max(organic_fertilizer, na.rm = TRUE),
         rotation_check=max(rotation, na.rm = TRUE ))|> # hh practices rotation in some maize plot
  ungroup() |> 
  relocate(rotation_check, .after = rotation)

### aggregating the information at the household level for adopters and non-adopters seperately.----
### 1. Select adopted plots If there are few intercropped fields select the biggest and 
### adjust if rotation was practiced in any field of the hh
### 2. Among the non-adopted plots select the biggest field. 
n_distinct(mlw1$plot_id) # 1336 but 1361 obs so duplicate ids
table(mlw1$intercrop, mlw1$rotation) # 960 non adopted plots
mlw1adoption <- mlw1 |> 
  relocate(hh_id_obs, .after = hhid) |> 
  mutate(keep=if_else(intercrop==1 & rotation==1, 1, 0))
# keep only adopting households; for non-adopters will select the biggest parcel and manager characteristics
mlw1adoption <- mlw1adoption |> 
  filter(!is.na(plot_area_GPS)) |> 
  group_by(hh_id_obs) |> 
  filter(intercrop != 0 | rotation != 0) |> 
  ungroup()
table(mlw1adoption$intercrop, mlw1adoption$rotation)
# there are only 5 observations where both rotation and intercrop are observed. this plots will be kept 
# first keep these plots and hhs
mlw1adoption <- mlw1adoption |> 
  group_by(hh_id_obs) |> 
  mutate(keephh=max(keep)) |> 
  ungroup()
mlw1adoption <- mlw1adoption |>
  filter((keep==0 & keephh==0) | (keep==1 & keephh==1)) 
mlw1adoption$keephh <- NULL
comment(mlw1adoption) <- "only adopter hhs and adopted plots"
mlw1adoption <- mlw1adoption |> 
  group_by(hh_id_obs) |> 
  filter(intercrop==max(intercrop, na.rm = TRUE )) |>  # there was only one parcel that had been rotated but not intercropped
  ungroup()
mlw1adoption <- mlw1adoption |>
  mutate(rotation=rotation_check) # one needed change for the indication of adoption
# keep the biggest plot
mlw1adoption <- mlw1adoption |> 
  group_by(hh_id_obs) |> 
  filter(plot_area_GPS == max(plot_area_GPS, na.rm = TRUE)) |> 
  ungroup()
n_distinct(mlw1adoption$hh_id_obs) # 308hhs, still some duplicate plots
mlw1adoption <- mlw1adoption %>%
  distinct(across(-plot_id), .keep_all = TRUE)
mlw1adoption <- mlw1adoption |> group_by(hh_id_obs) |> mutate(dupl=n()) |>
  relocate(dupl, .after=hhid) |>  ungroup()

# one more because of some minor difference in plot distance or soil type. select random here. 
mlw1adoption <- mlw1adoption |>
  group_by(hh_id_obs) |>  
  filter(
    (dupl > 1 & plot_dist_household==min(plot_dist_household, na.rm=TRUE)) |
      # if hh has no duplicates → keep all rows
      (dupl == 1)
  ) |> 
  ungroup()
# select the biggest plot among non-adopters
mlw1 <- mlw1 |> 
  filter(!is.na(plot_area_GPS)) |> # drop missing plots
  filter(intercrop == 0 & rotation == 0) 
mlw1 <- mlw1 |> 
  group_by(hh_id_obs) |> 
  filter(plot_area_GPS == max(plot_area_GPS, na.rm = TRUE)) |> 
  ungroup()
n_distinct(mlw1$hh_id_obs) # 763 hh
mlw1 <- mlw1 |> distinct() # 12 duplicates dropped but still some duplicates
mlw1 <- mlw1 |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs)
# investigating this duplicates we see that these are the same size
mlw1 <- mlw1 |> 
  group_by(hh_id_obs) |> 
  mutate(
    complete_row = complete.cases(pick(everything())) # TRUE if no NAs in this row
  ) |> 
  filter(
    # if hh has duplicates → keep only complete rows
    (dupl > 1 & complete_row) |
      # if hh has no duplicates → keep all rows
      (dupl == 1)
  ) %>%
  ungroup()
mlw1 <- mlw1 |> select(-dupl, -complete_row) |> distinct(hh_id_obs, .keep_all = TRUE) # 2 duplicates dropped
# Combine adopter and non-adopter hhs
mlw1 <- bind_rows(mlw1, mlw1adoption)
n_distinct(mlw1$hh_id_obs)
mlw1 <- mlw1 |> 
  group_by(hh_id_obs) |> mutate(dupl=n()) |> ungroup() |> 
  relocate(dupl, .after= hh_id_obs) |> 
  arrange(desc(dupl), hh_id_obs)
# There are hhs, which have adopter plots and non-adopted plots. keep only adopted plots
mlw1 <- mlw1 |> 
  group_by(hh_id_obs) |> 
  filter((dupl>1 & (intercrop==1 | rotation==1)) |
           dupl==1) |> ungroup()
# 1008 observations
mlw1 <- mlw1 |> 
  select(-dupl, -main_crop, -rotation_check, -keep) |> 
  arrange(hh_id_obs)
look_for(mlw1) 
var_label(mlw1$plot_area) <- "Area (in hectares) of all maize plots"
labs <- var_label(plot1)
common_names <- intersect(names(mlw1), names(labs))
var_label(mlw1)[common_names] <- labs[common_names]
look_for(mlw1) 
rm(labs, common_names)
mlw1 <- mlw1 |> 
  filter(plot_area!=0)
mlw1$plot_area_GPS <- NULL
look_for(mlw1)
mlw1 <- left_join(mlw1, sales, by="hhid")
var_label(mlw1$female_dm) <- "Who kept/decided what to do with crop income?"
mlw1 <- left_join(mlw1, hdds1, by=c("hhid"="hh_id_merge"))
mlw1 <- left_join(mlw1, individual, by=c("hhid"="case_id"))
mlw1 <- mlw1 |> 
  mutate(female_edu=if_else(is.na(female_edu), 0, female_edu))
remove(hdds1, plot1, individual)
mlw1 <- mlw1 |> mutate(wave="wave1")
rm(mlw1adoption)

## Cleaning missing data ----
look_for(mlw1)
mlw1 <- mlw1 |> select(-manager_id_merge)
### Subsistence orientation----
### share_kg_sold in HP data has many missing; reason: set to missing if reported share above 100% percent or
##  if household does not harvest any crops.
##  We will capture subsistence orientation: HHs do not sell any crops. 
sales <- read_dta("data/mlw/ag_mod_i_10.dta") |> 
  select(case_id, ag_i0b, ag_i01)
sales <- semi_join(sales, maizehh, by=c("case_id"="hh_id_merge"))
look_for(sales, details = "basic")
frq(sales$ag_i01)
sales <- sales |> mutate(subsistence_ag=if_else(ag_i01==2,1,0))
sales <- collap(sales, subsistence_ag ~ case_id, FUN = fmin, na.rm = TRUE ) # if some consider for sale then not a subsistence. 
sales <- sales |> rename(hhid=case_id)
mlw1 <- left_join(mlw1, sales, by="hhid")
frq(mlw1$subsistence_ag)
remove(sales)

### ### ### ### ### ### ### ###
# Other variables   ----
### ### ### ### ### ### ### ###
## Distance ----
community <- read_dta("data/mlw/com_cd_10.dta") |> 
  select(ea_id, com_cd18a , com_cd18b)
frq(community$com_cd18b) # 1 meter 2 km 3 mile
community <- community |> 
  mutate(market_distance=case_when(com_cd18b==1 ~ com_cd18a/1000,
                                   com_cd18b==3 ~ com_cd18a/0.62,
                                   .default = com_cd18a)) |> 
  select(ea_id, market_distance)
community %>%
  count(ea_id) %>%
  filter(n > 1) # distinct 
community <- community |> distinct(ea_id,.keep_all = TRUE)
mlw1 <- left_join(mlw1, community, by=c("ea_id_merge"="ea_id"))
## Non-farm income ----
wage <- read_dta("data/mlw/hh_mod_e_10.dta")
look_for(wage, details = c("none"))
wage <- wage[,c(2, 28:29, 33:41,  47:48, 51:59, 76:79 )]
wage <- semi_join(wage, maizehh, by=c("case_id"="hh_id_merge"))
look_for(wage, details = c("none"))
mainwage <- wage[,1:12]
mainwage <- mainwage |> filter(!is.na(hh_e19b))
frq(mainwage$hh_e26b)
mainwage <- mainwage |> 
  mutate(main_income=case_when(hh_e26b==5 ~ hh_e25 * hh_e22/hh_e26a,
                               hh_e26b==4 ~ hh_e25 * hh_e22 * hh_e23 /hh_e26a,
                               hh_e26b==3 ~ hh_e25 * (hh_e22 * hh_e23 * hh_e24/8) / hh_e26a,
                               .default = NA_real_)) |>  # very rough calculation 
  mutate(main_income=as.numeric(main_income))
# 2 missing; 18 days and one week of employment salary missing 
mainwage$main_income[is.na(mainwage$main_income)] <- 0
look_for(mainwage)
frq(mainwage$hh_e28b)
mainwage <- mainwage |> 
  mutate(allowance=case_when(hh_e28b==5 ~ hh_e27 * hh_e22 / hh_e28a,
                             hh_e28b==4 ~ hh_e27 * hh_e22 * hh_e23 / hh_e28a,
                             hh_e28b==3 ~ hh_e27 * (hh_e22 * hh_e23 * hh_e24/8) / hh_e28a,
                             .default = NA_real_)) |>  # very rough calculation 
  mutate(allowance=as.numeric(allowance))
mainwage <- mainwage |> mutate(income=rowSums(across(c(main_income,allowance)), na.rm=TRUE))
mainwage <- mainwage |> select(case_id, income)
look_for(wage, details = c("none"))
#secondwage
secondwage <- wage[,c(1,13:23)]
look_for(secondwage)
secondwage <- secondwage |> filter(!is.na(hh_e36))
frq(secondwage$hh_e40b) # 7 obs
secondwage <- secondwage |>
  mutate(main_income=case_when(hh_e40b==5 ~ hh_e39 * hh_e36/hh_e40a,
                               hh_e40b==4 ~ hh_e39 * hh_e36 * hh_e37 /hh_e40a,
                               hh_e40b==3 ~ hh_e39 * (hh_e36 * hh_e37 * hh_e38/8) /hh_e40a,
                               .default = NA_real_)) |> 
  mutate(main_income=as.numeric(main_income))
frq(secondwage$hh_e42b) # 1 obs
secondwage <- secondwage |> 
  mutate(allowance=case_when(hh_e42b==4 ~ hh_e41 * hh_e36 * hh_e37 / hh_e40a,
                             .default = NA_real_))  # very rough calculation 
secondwage <- secondwage |> mutate(income=rowSums(across(c(main_income,allowance)), na.rm=TRUE)) |> 
  select(case_id, income)
look_for(wage, details = c("none"))
#ganyu
ganyu <- wage[,c(1,24:27)]
look_for(ganyu)
ganyu <- ganyu |> filter(!is.na(hh_e56))
ganyu <- ganyu |> 
  mutate(income=hh_e56*hh_e57*hh_e58*hh_e59) # some missing becuase weeks are missing
# will just adopt e59
ganyu <- ganyu |> 
  mutate(income=case_when(is.na(income) ~ hh_e59,
                          .default = income))
ganyu <- ganyu |> filter(!is.na(ganyu$income))
ganyu <- collap(ganyu,income~case_id, FUN=fsum)
wage <- rbind(ganyu, mainwage, secondwage)
remove(ganyu, mainwage, secondwage)
wage <- collap(wage,income~case_id, FUN=fsum)
mlw1 <- left_join(mlw1, wage, by=c("hhid"="case_id"))
mlw1 <- mlw1 |> 
  tidyr::replace_na(list(income = 0)) |> 
  rename(wage_income=income)
var_label(mlw1$wage_income) <- "income from main and temporary off-farm work"
var_label(mlw1$subsistence_ag) <- "HH did not sell any crop harvest"
var_label(mlw1$female_edu) <- "HH has an educated adult female"
remove(wage)

# enterprise
enterprise <- read_dta("data/mlw/hh_mod_n2_10.dta")
look_for(enterprise, details=c("none"))
enterprise <- semi_join(enterprise,  maizehh, by=c("case_id"="hh_id_merge"))
# It is not possible to calculate monthly profits from the enterprise.
# We will take the last month's profit. 
# Enterprise data is better calculated for the last month's profit. An annual income cannot be precisely calculated.
# Thus, to sum all other income, we must consider average monthly income for the wage income as well 
# (unlike in ETH where yearly is calculated). 
enterprise <- enterprise |> 
  filter(!is.na(hh_n40)) |> 
  rename(enterprise_income=hh_n40)
enterprise <- collap(enterprise, enterprise_income ~ case_id, FUN=fsum)  

# other income
other_income <- read_dta("data/mlw/hh_mod_p_10.dta") |> 
  filter(hh_p01==1)
other_income <- semi_join(other_income,  maizehh, by=c("case_id"="hh_id_merge"))
look_for(other_income, details = c("none"))
other_income <- other_income |> 
  mutate(other_income=rowSums(across(c(hh_p02, hh_p03a, hh_p03b, hh_p03c)), na.rm=TRUE))
other_income <- collap(other_income, other_income ~ case_id, FUN=fsum)
# other income is yearly,
# wage_income is yearly,
# enterprise is but last month. 
mlw1 <- left_join(mlw1, other_income, by=c("hhid"="case_id"))
mlw1 <- mlw1 |> 
  tidyr::replace_na(list(other_income = 0))
mlw1 <- left_join(mlw1, enterprise, by=c("hhid"="case_id"))
mlw1 <- mlw1 |> 
  tidyr::replace_na(list(enterprise_income = 0))
mlw1 <- mlw1 |> 
  mutate(other_income=other_income / 12,
         wage_income=wage_income / 12,
         tot_other_income=wage_income+other_income+enterprise_income)
var_label(mlw1$tot_other_income) <- "Average monthly total other income"
remove(enterprise, other_income, community)
look_for(mlw1)

##  Extension, advisory, credit----
# We will consider most important sources like government and private ag. extension.
extension <- read_dta("data/mlw/ag_mod_t1_10.dta") |> 
  select(case_id, ag_t0a, ag_t01, ag_t02) |> 
  filter(ag_t01==1)
frq(extension$ag_t0a)
frq(extension$ag_t02)
# Many report to have received extension from electronic media like tv and radio and other farmers. 
# We will consider those extension services that have been provided by gov, private and cooperative type institutions. 
extension <- extension |> 
  mutate(extension_service=case_when(ag_t0a %in% c(301:307, 309) &
                                       ag_t02 %in% c(1:10) ~ 1,
                                     .default = 0))
extension <- collap(extension, extension_service~case_id, FUN = fmax)
frq(extension$extension_service)
mlw1 <- left_join(mlw1, extension, by=c("hhid"="case_id"))
mlw1$extension_service[is.na(mlw1$extension_service)] <- 0
var_label(mlw1$extension_service) <- "received extension from institutions" 
remove(extension)

### Credit ----
credit <- read_dta("data/mlw/hh_mod_s2_10.dta") |> 
  select(case_id, hh_s12, hh_s19a, hh_s19b)
frq(credit$hh_s19a) # do not know any lender; this is what we can capture
credit <- credit |> 
  mutate(credit_constrained=case_when(
    hh_s19a %in% c(2,5,7) ~ 1,
    !is.na(hh_s19a) ~ 0,
    .default = NA_integer_)) # only looking at the main reasons
credit <- credit |> 
  filter(!is.na(credit_constrained)) |> 
  select(case_id, credit_constrained)
mlw1 <- left_join(mlw1, credit, by=c("hhid"="case_id"))
mlw1$credit_constrained[is.na(mlw1$credit_constrained)] <- 0
frq(mlw1$credit_constrained)
remove(credit)
var_label(mlw1$credit_constrained) <- "becuase no collateral or no access to lender"

## TLU ----
## page 10: https://appec-h.com/wp-content/uploads/2023/02/Livestock-production-systems.pdf
## Large ruminants = 0.7 TLU, small ruminants = 0.1, pigs = 0.2, poultry = 0.01, horses = 0.8,
## mules=  0.7, camels= 1
tlu <- read_dta("data/mlw/ag_mod_r1_10.dta") |> 
  select(case_id, ag_r00, ag_r0a, ag_r01, ag_r02) |> 
  filter(ag_r01==1)
frq(tlu$ag_r0a)
tlu <- tlu |> 
  mutate(tlu=case_when(ag_r0a %in% c(301:304) ~ ag_r02 * 0.7,
                       ag_r0a %in% c(305:306) ~ ag_r02 * 0.7,
                       ag_r0a %in% c(307:308) ~ ag_r02 * 0.1,
                       ag_r0a==309 ~ ag_r02 * 0.2,
                       ag_r0a %in% c(310:316) ~ ag_r02 * 0.01,
                       .default = 0))
tlu <- collap(tlu, tlu ~ case_id, FUN = fsum)
mlw1 <- left_join(mlw1, tlu, by=c("hhid"="case_id"))
mlw1$tlu[is.na(mlw1$tlu)] <- 0
remove(tlu)
mlw1 <- mlw1 |> 
  rename(ea_id=ea_id_merge)

## Land tenure - not collected ----
## We will use information about MOST recent disputes from 2016 data. 
## This is not perfect and likely has a measurement error, because the question in 2016 asks about the most recent dispute.
## Thus hh's with consistent disputes or multiple disputes may not be correctly captured with below approach. 
## However, this is the only available data we have. 
tenure <- read_dta("data/mlw/ag_mod_v_16.dta") |> 
  select(y3_hhid, ag_v02_1) |> 
  filter(!is.na(ag_v02_1)) |> 
  filter(ag_v02_1<2011) # keep those disputes before 2011 for first wave
n_distinct(tenure$y3_hhid)
look_for(tenure)
tenure <- collap(tenure, ag_v02_1 ~y3_hhid, FUN = fmax, na.rm=TRUE)
maizehh16 <- read_dta("data/hp/Plot_dataset.dta") |>
  filter(country=="Malawi", wave==3) |> 
  filter(main_crop=="MAIZE") |> 
  distinct(hh_id_merge, hh_id_obs)
tenure16 <- left_join(maizehh16, tenure, by=c("hh_id_merge"="y3_hhid"))
remove(tenure, maizehh16)
tenure16 <- tenure16 |> select(hh_id_obs, ag_v02_1)
mlw1 <- left_join(mlw1, tenure16, by="hh_id_obs")
# If hhs did not have any disputes before 2010, we will assume that these hhs are very likely to possess land in 10 years too.
mlw1 <- mlw1 |>
  mutate(tenure_security=(case_when(is.na(ag_v02_1) ~ 1,
                                     .default = 0))) |> 
  select(-ag_v02_1)
var_label(mlw1$tenure_security) <- "very likely to still possess land in ten years"  
remove(tenure16)

## Input coupon ---- 
coupon <- read_dta("data/mlw/ag_mod_e_10.dta") |> 
  filter(ag_e0a==1) |>  # obtained coupon
  select(case_id, ag_e0d, ag_e07)
look_for(coupon)
frq(coupon$ag_e0d) 
# 23:21:0+4S/Chitowe is a government-issued voucher that entitles the holder to purchase NPK
frq(coupon$ag_e07)
#  coupon |> 
#   filter(ag_e0d==11) |> 
#   select(ag_e07) |> 
#   print(n=40) # other chemical
coupon <- coupon |>
  mutate(fert_voucher=if_else(as.integer(ag_e07) %in% c(1:6), 1, 0),
         maize_voucher=if_else(as.integer(ag_e07)==12, 1, 0))
n_distinct(coupon$case_id)  
coupon <- collap(coupon, maize_voucher + fert_voucher ~ case_id, FUN = fmax, na.rm=TRUE)
mlw1 <- left_join(mlw1, coupon, by=c("hhid"="case_id"))
mlw1$maize_voucher[is.na(mlw1$maize_voucher)] <- 0
mlw1$fert_voucher[is.na(mlw1$fert_voucher)] <- 0
frq(mlw1$maize_voucher)
frq(mlw1$fert_voucher)
var_label(mlw1$maize_voucher) <- "HH had obtained vaicher for maize seed"
var_label(mlw1$fert_voucher) <- "HH had obtained vaicher for inorganic fertilizer"
remove(coupon)

## HH head gender ----
head <- read_dta("data/mlw/hh_mod_b_10.dta") |> 
  select(case_id, hh_b03, hh_b04 ) |> 
  filter(hh_b04==1) # keep only head
frq(head$hh_b04)
frq(head$hh_b03)
head <- head |> 
  mutate(female_head=if_else(hh_b03==2,1,0)) |> 
  select(case_id, female_head)
mlw1 <- left_join(mlw1, head, by=c("hhid"="case_id"))
rm(head)

## Other crops produced -----
garden <- read_dta("data/mlw/ag_mod_d_10.dta") |>   # plot level 
  select(case_id, ag_d14, ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e) |> 
  filter(ag_d14==1)
look_for(garden)
garden <- semi_join(garden, maizehh, by=c("case_id"="hh_id_merge"))
garden <- garden |> 
  rowwise() |> 
  mutate(
    other_crops_count = sum(
      !is.na(c_across(c(ag_d20a, ag_d20b, ag_d20c, ag_d20d, ag_d20e))) & 
      !(c_across(ag_d20a:ag_d20e) %in% c(1, 11:16, 27, 34:36, 46))  # not maize or legumes
      )
      ) %>%
  ungroup()
garden <- garden |> 
  group_by(case_id) |> 
  summarise(other_crops=sum(other_crops_count)) |> 
  ungroup()
mlw1 <- left_join(mlw1, garden, by=c("hhid"="case_id"))
frq(mlw1$other_crops)
var_label(mlw1$other_crops) <- "number of other crops produced"
rm(maizehh, garden)

## Rainfall lsms ----
rain <- read_dta("data/mlw/HouseholdGeovariables_IHS3_Rerelease_10.dta") |> 
  select(case_id, anntot_avg)
look_for(rain)
mlw1 <- left_join(mlw1, rain, by=c("hhid"="case_id"))
rm(rain)


## Missing data and notes ----
## Some do not have agricultural assets but still have asset index. This is because, pca (factor) analysis still includes those with 
## no asset and gives them a lower score.
## Flood shock is absent in mlw1, some missing in other waves are assumed no shock. 








